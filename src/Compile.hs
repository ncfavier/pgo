{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (compileFile, TypeError(..)) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State.Strict
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe

import AST hiding (structures, functions)
import Pack
import X86_64

instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (StateT s' m) where
    get = lift get
    put = lift . put

data TypeError = TypeError Location String deriving Show

throw e = throwError (TypeError nowhere e)
throwAt l e = throwError (TypeError l e)

data GlobalState = GlobalState { structures   :: Map String Structure
                               , functions    :: Map String Function
                               , fmtImported  :: Bool
                               , fmtUsed      :: Bool
                               , labelCounter :: Integer
                               , strings      :: Map String String
                            -- , labels       :: ?
                               }

type Structure = Pack Type

data FunctionState = FunctionState { currentFunction :: (Identifier, Function)
                                   , scopes          :: [Scope]
                                   , returned        :: Bool
                                   }

data LocalVar = LocalVar { varType  :: Type
                         , used     :: Bool
                         , location :: Location
                         }

type Scope = Pack LocalVar

type Compiler = StateT GlobalState (WriterT String (Except TypeError))

type FunctionCompiler = StateT FunctionState Compiler

getStructure s = (M.! s) <$> gets structures

setFmtUsed = modify' \gs -> gs { fmtUsed = True }

getStringLiteral "" = return (imm 0)
getStringLiteral s = immLabel <$> do
    strings <- gets strings
    case M.lookup s strings of
        Just l -> return l
        Nothing -> do
            c <- gets labelCounter
            let l = ".LS" ++ show c
            modify' \gs -> gs { strings = M.insert s l strings }
            return l

getBottom = gets (bottom . head . scopes)

getLocalVar :: Identifier -> FunctionCompiler (Integer, LocalVar)
getLocalVar (v :@ l) = do
    scopes <- gets scopes
    let (m, scopes') = mapAccumL f Nothing scopes
    modify' \fs -> fs { scopes = scopes' }
    maybe (throwAt l $ "unbound variable " ++ v) return m
    where
        f Nothing s | Just (o, lv) <- getValue v s = let lv' = (o, lv { used = True })
                                                     in (Just lv', s { objects = M.insert v lv' (objects s) })
                    | otherwise = (Nothing, s)
        f m s = (m, s)

pushScope s = modify' \fs -> fs { scopes = s:scopes fs }
popScope = modify' \fs -> fs { scopes = tail (scopes fs) }

addLocalVar :: Type -> Identifier -> FunctionCompiler ()
addLocalVar _ ("_" :@ _) = return ()
addLocalVar t (v :@ l) = do
    s <- sizeof t
    ~(sc:scs) <- gets scopes
    case pushDown v (LocalVar t False l) s sc of
        Just sc' -> modify' \fs -> fs { scopes = sc':scs }
        Nothing -> throwAt l $ "redefined variable " ++ v

setReturned r = modify' \fs -> fs { returned = r }

builtinTypes = [("int", 8), ("bool", 8), ("string", 8)]

sizeof :: (MonadState GlobalState m, MonadError TypeError m) => Type -> m Integer
sizeof (Type (t :@ l)) = do
    structures <- gets structures
    if | Just s <- M.lookup t structures -> return (size s)
       | Just s <- lookup t builtinTypes -> return s
       | otherwise                       -> throwAt l $ "invalid type " ++ t
sizeof (Pointer t) = 8 <$ sizeof t

addStructures :: [(Identifier, Fields)] -> Compiler ()
addStructures structuresList = do
    let addRawStructure ss (n :@ l, s) = do
            when (n `M.member` ss) $ throwAt l $ "duplicate structure " ++ n
            return $ M.insert n (s, l) ss
    rawStructures <- foldM addRawStructure M.empty structuresList
    let addStructure = go [] where
            go seen n = do
                let (rawStruct, l) = rawStructures M.! n
                when (n `elem` seen) $ throwAt l $ "recursive structures " ++ intercalate ", " seen
                let addField s (f :@ l, t) = do
                        structures <- gets structures
                        case t of
                            Type (n' :@ _) | n' `M.member` rawStructures && not (n' `M.member` structures) -> go (n:seen) n'
                            _ -> return ()
                        s' <- sizeof t
                        maybe (throwAt l $ "duplicate field " ++ f ++ " in structure " ++ n) return $ pushUp f t s' s
                structure <- foldM addField emptyPack rawStruct
                modify' \gs -> gs { structures = M.insert n structure (structures gs) }
    mapM_ addStructure (M.keysSet rawStructures)

addFunctions :: [(Identifier, Function)] -> Compiler ()
addFunctions = mapM_ \(n :@ l, f) -> do
    functions <- gets functions
    when (n `M.member` functions) $ throwAt l $ "duplicate function " ++ n
    modify' \gs -> gs { functions = M.insert n f functions }

function f = "pgo_func_" ++ f

entryPoint :: Compiler ()
entryPoint = do
    global "main"
    label "main"
    call (function "main")
    zero rax
    ret

compileFunction :: (Identifier, Function) -> Compiler ()
compileFunction f@(n :@ l, Function parameters returns body) = do
    when (n == "main") do
        unless (null parameters) $ throwAt l "main should not take any parameters"
        unless (null returns) $ throwAt l "main should not return anything"
    label (function n)
    push rbp
    mov rsp rbp
    let addParameter (v :@ l, t) s = do
            s' <- sizeof t
            maybe (throwAt l $ "duplicate parameter " ++ v ++ " in function " ++ n) return $ pushUp v (LocalVar t True nowhere) s' s
    paramsScope <- foldrM addParameter (emptyPackOffset 16) parameters
    let paramsSize = size paramsScope
        initialFunctionState = FunctionState f [paramsScope { bottom = 0 }] False
    FunctionState{..} <- execStateT (mapM_ compileStatement body >> checkUnused) initialFunctionState
    when (length returns > 0 && not returned) $ throwAt l $ "missing return statement in function " ++ n
    leave
    ret1 (imm paramsSize)

checkUnused :: FunctionCompiler ()
checkUnused = do
    vars <- gets (objects . head . scopes)
    () <$ M.traverseWithKey f vars
    where
        f v (_, LocalVar { used = False, location = l }) = throwAt l $ "unused variable " ++ v
        f _ _ = return ()

compileBlock :: [Statement] -> FunctionCompiler ()
compileBlock b = do
    bottom <- getBottom
    pushScope (emptyPackOffset bottom)
    mapM_ compileStatement b
    checkUnused
    popScope

compileStatement :: Statement -> FunctionCompiler ()
compileStatement (Block b) = compileBlock b
compileStatement (Expression e) = do
    compileExpression e
    bottom <- getBottom
    lea (bottom `rel` rbp) rsp
compileStatement (Increment e) = do
    compileLeftValueAsType "int" e
compileStatement (Decrement e) = do
    compileLeftValueAsType "int" e
compileStatement (Var vs (Just t) ([] :@ _)) = do
    mapM_ (addLocalVar t) vs
compileStatement (Var vs (Just t) es) = do
    compileExpressionsAsTypes (t <$ vs) es
    mapM_ (addLocalVar t) vs
compileStatement (Var vs Nothing es) = do
    ts <- compileExpressions es
    zipWithM_ addLocalVar ts vs
compileStatement (Assign vs es) = do
    ts <- mapM compileLeftValue vs
    compileExpressionsAsTypes ts es
compileStatement (Return es) = do
    ts <- gets (returns . snd . currentFunction)
    compileExpressionsAsTypes ts es
    setReturned True
compileStatement (If cond yes no) = do
    compileExpressionAsType "bool" cond
    r0 <- gets returned
    compileBlock yes
    r1 <- gets returned
    setReturned r0
    compileBlock no
    r2 <- gets returned
    setReturned (r1 && r2)
compileStatement (For cond b) = do
    compileExpressionAsType "bool" cond
    r0 <- gets returned
    compileBlock b
    setReturned r0

compileLeftValue :: Expression -> FunctionCompiler Type
compileLeftValue (Variable v :@ _) = do
    scopes <- gets scopes
    (offset, LocalVar{..}) <- getLocalVar v
    return varType
compileLeftValue (Dot (e :@ le) (m :@ lm) :@ _) = do
    t <- compileLeftValue (e :@ le)
    s :@ _ <- case t of
        Type t -> return t
        Pointer (Type t) -> return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (_, t') <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return . getValue m =<< getStructure s
    return t'
compileLeftValue (Unary ("*" :@ _) (e :@ l) :@ _) = do
    t <- compileLeftValue (e :@ l)
    case t of
        Pointer t' -> return t'
        _ -> throwAt l $ "cannot dereference a value of type " ++ show t
compileLeftValue (e :@ l) = throwAt l "this expression is not a left value"

compileLeftValueAsType :: Type -> Expression -> FunctionCompiler ()
compileLeftValueAsType t e@(_ :@ l) = do
    t' <- compileLeftValue e
    unless (t `matches` t') $ throwAt l $ "this value has type " ++ show t' ++ " but was expected of type " ++ show t

compileExpression :: Expression -> FunctionCompiler [Type]
compileExpression (Int i :@ _) = do
    push (imm i)
    return ["int"]
compileExpression (String "" :@ _) = do
    push (imm 0)
    return ["string"]
compileExpression (String s :@ _) = do
    push =<< getStringLiteral s
    return ["string"]
compileExpression (Bool b :@ _) = do
    push (imm (if b then 1 else 0))
    return ["bool"]
compileExpression (Nil :@ _) = do
    push (imm 0)
    return [NilType]
compileExpression (Variable ("_" :@ l) :@ _) = throwAt l "the variable _ has no value"
compileExpression v@(Variable _ :@ _) = do
    t <- compileLeftValue v
    return [t]
compileExpression (Dot e@(_ :@ le) (m :@ lm) :@ _) = do
    t <- compileSimpleExpression e
    s :@ _ <- case t of
        Type t -> return t
        Pointer (Type t) -> return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (_, t') <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return . getValue m =<< getStructure s
    return [t']
compileExpression (Call ("new" :@ _) ([Variable (s :@ l) :@ _] :@ _) :@ _) = do
    structures <- gets structures
    if isJust (lookup s builtinTypes) || M.member s structures then
        return [Pointer (Type (s :@ nowhere))]
    else
        throwAt l $ "no such structure " ++ s
compileExpression (Call ("new" :@ _) (_ :@ l) :@ _) = do
    throwAt l "new() expects a single type name"
compileExpression (Call (f :@ l) es :@ _) = do
    Function{..} <- maybe (throwAt l $ "undefined function " ++ f) return . M.lookup f =<< gets functions
    let ts = map snd parameters
    compileExpressionsAsTypes ts es
    return returns
compileExpression (Print es :@ l) = do
    fmtImported <- gets fmtImported
    unless fmtImported $ throwAt l "fmt used but not imported"
    setFmtUsed
    compileExpressions es
    return []
compileExpression (Unary ("!" :@ _) e :@ _) = do
    compileExpressionAsType "bool" e
    return ["bool"]
compileExpression (Unary ("-" :@ _) e :@ _) = do
    compileExpressionAsType "int" e
    return ["int"]
compileExpression (Unary ("*" :@ _) e@(_ :@ l) :@ _) = do
    t <- compileSimpleExpression e
    case t of
        Pointer t' -> return [t']
        _ -> throwAt l $ "cannot dereference type " ++ show t
compileExpression (Unary ("&" :@ _) e :@ _) = do
    t <- compileLeftValue e
    return [Pointer t]
compileExpression (Unary (op :@ l) _ :@ _) = throwAt l $ "unknown unary operator " ++ op
compileExpression (Binary (op :@ _) (Nil :@ _) (Nil :@ _) :@ l) | op `elem` ["==", "!="] = do
    throwAt l "cannot compare nil with nil"
compileExpression (Binary (op :@ _) e1 e2 :@ l) | op `elem` ["==", "!="] = do
    t1 <- compileSimpleExpression e1
    t2 <- compileSimpleExpression e2
    unless (t1 `matches` t2) $ throwAt l $ "cannot match types " ++ show t1 ++ " and " ++ show t2
    return ["bool"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["<", "<=", ">", ">="] = do
    compileExpressionAsType "int" e1
    compileExpressionAsType "int" e2
    return ["bool"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["+", "-", "*", "/", "%"] = do
    compileExpressionAsType "int" e1
    compileExpressionAsType "int" e2
    return ["int"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["&&", "||"] = do
    compileExpressionAsType "bool" e1
    compileExpressionAsType "bool" e2
    return ["bool"]
compileExpression (Binary (op :@ l) _ _ :@ _) = throwAt l $ "unknown binary operator " ++ op

compileSimpleExpression :: Expression -> FunctionCompiler Type
compileSimpleExpression e@(_ :@ l) = do
    ts <- compileExpression e
    case ts of
        [t] -> return t
        [] -> throwAt l $ "this expression has no value but a single value was expected"
        _ -> throwAt l $ "this expression returns " ++ show (length ts) ++ " values but a single value was expected"

compileExpressionAsType :: Type -> Expression -> FunctionCompiler ()
compileExpressionAsType t e@(_ :@ l) = do
    t' <- compileSimpleExpression e
    unless (t' `matches` t) $ throwAt l $ "this expression has type " ++ show t' ++ " but was expected of type " ++ show t

compileExpressions :: Expressions -> FunctionCompiler [Type]
compileExpressions ([e@(Call _ _ :@ _)] :@ _) = compileExpression e
compileExpressions (es :@ l) = mapM compileSimpleExpression es

compileExpressionsAsTypes :: [Type] -> Expressions -> FunctionCompiler ()
compileExpressionsAsTypes ts es@(_ :@ l) = do
    ts' <- compileExpressions es
    unless (length ts' == length ts) $ throwAt l $ "these expressions return " ++ show (length ts') ++ " values but " ++ show (length ts) ++ " values were expected"

matches :: Type -> Type -> Bool
NilType   `matches` NilType   = False
NilType   `matches` Pointer _ = True
Pointer _ `matches` NilType   = True
a         `matches` b         = a == b

compileStringLiterals :: Compiler ()
compileStringLiterals = do
    strings <- gets strings
    () <$ flip M.traverseWithKey strings \s l -> do
        label l
        string s

compileFile :: File -> Either TypeError String
compileFile (File importFmt structuresList functionsList) =
    runExcept $ execWriterT $ flip evalStateT initialGlobalState do
        addStructures structuresList
        addFunctions functionsList
        text
        entryPoint
        mapM_ compileFunction functionsList
        data_
        compileStringLiterals
        functions <- gets functions
        unless ("main" `M.member` functions) $ throw "no main function"
        fmtUsed <- gets fmtUsed
        case importFmt of
            Just (() :@ l) | not fmtUsed -> throwAt l "module 'fmt' imported but not used"
            _ -> return ()
    where
        initialGlobalState = GlobalState { structures   = M.empty
                                         , functions    = M.empty
                                         , fmtImported  = isJust importFmt
                                         , fmtUsed      = False
                                         , labelCounter = 0
                                         , strings      = M.empty
                                         }
