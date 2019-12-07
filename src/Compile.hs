{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (compileFile) where

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

data TypeError = InvalidType String
               | DuplicateStructure String
               | RecursiveStructures [String]
               | DuplicateField String String
               | DuplicateFunction String
               | NoMain | MainParameters | MainReturn
               | DuplicateParameter String String
               | DuplicateLocalVariable String
               | UnboundVariable String
               | UndefinedFunction String
               | BadArity
               | WrongType [Type] [Type]
               | WrongTypeForNil Type
               | InvalidDotType Type
               | NoMemberInType String String
               | WrongArgumentsForNew
               | NoSuchStructure String
               | CannotDereference Type
               | CannotCompare Type
               | CannotMatchNils
               | CannotMatchTypes Type Type
               | ExpectedSingleType [Type]
               | NoConcreteTypeForNil
               | NoReturn
               | UnusedVariable String
               | Underscore
               | FmtNotImported | FmtNotUsed
               deriving Show

throw = throwError

data GlobalState = GlobalState { structures   :: Map String Structure
                               , functions    :: Map String Function
                               , fmtImported  :: Bool
                               , fmtUsed      :: Bool
                               , labelCounter :: Integer
                               , strings      :: Map String String
                            -- , labels       :: ?
                               }

type Structure = Pack Type

data FunctionState = FunctionState { currentFunction :: (String, Function)
                                   , scopes          :: [Scope]
                                   , returned        :: Bool
                                   }

data LocalVar = LocalVar { varType :: Type
                         , used    :: Bool
                         }

type Scope = Pack LocalVar

type Compiler = StateT GlobalState (WriterT String (Except TypeError))

type FunctionCompiler = StateT FunctionState Compiler

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

getVariable :: String -> FunctionCompiler (Integer, LocalVar)
getVariable v = do
    scopes <- gets scopes
    let (m, scopes') = mapAccumL f Nothing scopes
    modify' \fs -> fs { scopes = scopes' }
    maybe (throw (UnboundVariable v)) return m
    where
        f Nothing s = do
            case getValue v s of
                Just (o, lv) -> let lv' = (o, lv { used = True }) in
                    (Just lv', s { objects = M.insert v lv' (objects s) })
                Nothing -> (Nothing, s)
        f m s = (m, s)

pushScope s = modify' \fs -> fs { scopes = s:scopes fs }
popScope = modify' \fs -> fs { scopes = tail (scopes fs) }

addVariable :: String -> Type -> FunctionCompiler ()
addVariable "_" _ = return ()
addVariable v t = do
    s <- sizeof t
    ~(sc:scs) <- gets scopes
    case pushDown v (LocalVar t False) s sc of
        Just sc' -> modify' \fs -> fs { scopes = sc':scs }
        Nothing -> throw $ DuplicateLocalVariable v

setReturned r = modify' \fs -> fs { returned = r }

builtinTypes = [("int", 8), ("bool", 8), ("string", 8)]

sizeof :: (MonadState GlobalState m, MonadError TypeError m) => Type -> m Integer
sizeof (Type t) = do
    structures <- gets structures
    if | Just s <- M.lookup t structures -> return (size s)
       | Just s <- lookup t builtinTypes -> return s
       | otherwise                       -> throw (InvalidType t)
sizeof (Pointer _) = return 8

addStructures :: [(String, Fields)] -> Compiler ()
addStructures structuresList = do
    let addRawStructure ss (n, s) = do
            when (n `M.member` ss) $ throw $ DuplicateStructure n
            return $ M.insert n s ss
    rawStructures <- foldM addRawStructure M.empty structuresList
    let addStructure = go [] where
            go seen n = do
                when (n `elem` seen) $ throw $ RecursiveStructures seen
                let addField s (f, t) = do
                        structures <- gets structures
                        case t of
                            Type n' | n' `M.member` rawStructures && not (n' `M.member` structures) -> go (n:seen) n'
                            _ -> return ()
                        s' <- sizeof t
                        maybe (throw (DuplicateField n f)) return $ pushUp f t s' s
                structure <- foldM addField emptyPack (rawStructures M.! n)
                modify' \gs -> gs { structures = M.insert n structure (structures gs) }
    mapM_ addStructure (M.keysSet rawStructures)

addFunctions :: [(String, Function)] -> Compiler ()
addFunctions = mapM_ \(n, f) -> do
    functions <- gets functions
    when (n `M.member` functions) $ throw $ DuplicateFunction n
    modify' \gs -> gs { functions = M.insert n f functions }

function f = "pgo_func_" ++ f

entryPoint :: Compiler ()
entryPoint = do
    global "main"
    label "main"
    call (function "main")
    zero rax
    ret

discard :: FunctionCompiler ()
discard = do
    bottom <- getBottom
    lea (bottom `rel` rbp) rsp

compileFunction :: (String, Function) -> Compiler ()
compileFunction f@(n, Function parameters returns body) = do
    when (n == "main") do
        unless (null parameters) $ throw MainParameters
        unless (null returns) $ throw MainReturn
    label (function n)
    push rbp
    mov rsp rbp
    let addParameter (p, t) s = do
            s' <- sizeof t
            maybe (throw (DuplicateParameter n p)) return $ pushUp p (LocalVar t True) s' s
    paramsScope <- foldrM addParameter (emptyPackOffset 16) parameters
    let paramsSize = size paramsScope
        initialFunctionState = FunctionState f [paramsScope { bottom = 0 }] False
    FunctionState{..} <- execStateT (mapM_ compileStatement body >> checkUnused) initialFunctionState
    when (length returns > 0 && not returned) $ throw NoReturn
    leave
    ret1 (imm paramsSize)

checkUnused :: FunctionCompiler ()
checkUnused = do
    vars <- gets (objects . head . scopes)
    () <$ M.traverseWithKey f vars
    where
        f v (_, LocalVar { used = False }) = throw $ UnusedVariable v
        f _ _ = return ()

compileBlock :: [Statement] -> FunctionCompiler ()
compileBlock ss = do
    bottom <- getBottom
    pushScope (emptyPackOffset bottom)
    mapM_ compileStatement ss
    checkUnused
    popScope

compileStatement :: Statement -> FunctionCompiler ()
compileStatement (Block b) = compileBlock b
compileStatement (Expression e) = do
    compileExpression e
    discard
compileStatement (Increment e) = do
    t <- compileLeftValue e
    matchTypes t "int"
compileStatement (Decrement e) = do
    t <- compileLeftValue e
    matchTypes t "int"
compileStatement (Var vs (Just t) []) = do
    mapM_ (flip addVariable t) vs
compileStatement (Var vs (Just t) es) = do
    ts <- mapM compileExpression es
    ts <- case ts of
        [t] | length vs > 1 -> return t
        _ -> mapM singleType ts
    unless (length ts == length vs) $ throw BadArity
    mapM_ (matchTypes t) ts
    mapM_ (flip addVariable t) vs
compileStatement (Var vs Nothing es) = do
    ts <- mapM compileExpression es
    ts <- case ts of
        [t] | length vs > 1 -> return t
        _ -> mapM singleConcreteType ts
    unless (length ts == length vs) $ throw BadArity
    zipWithM_ addVariable vs ts
compileStatement (Assign vs es) = do
    vts <- mapM compileLeftValue vs
    ts <- mapM compileExpression es
    ts <- case ts of
        [t] | length vs > 1 -> return t
        _ -> mapM singleType ts
    unless (length ts == length vs) $ throw BadArity
    zipWithM_ matchTypes vts ts
compileStatement (Return es) = do
    rts <- gets (returns . snd . currentFunction)
    ts <- mapM compileExpression es
    ts <- case ts of
        [t] | length rts > 1 -> return t
        _ -> mapM singleType ts
    unless (length ts == length rts) $ throw BadArity
    zipWithM_ matchTypes rts ts
    setReturned True
compileStatement (If cond yes no) = do
    tc <- singleType =<< compileExpression cond
    matchTypes "bool" tc
    r0 <- gets returned
    compileBlock yes
    r1 <- gets returned
    setReturned r0
    compileBlock no
    r2 <- gets returned
    setReturned (r1 && r2)
compileStatement (For cond b) = do
    tc <- singleType =<< compileExpression cond
    matchTypes "bool" tc
    r0 <- gets returned
    compileBlock b
    setReturned r0

compileLeftValue :: Expression -> FunctionCompiler Type
compileLeftValue (Variable v) = do
    scopes <- gets scopes
    (offset, lv) <- getVariable v
    return (varType lv)
compileLeftValue (Dot e m) = do
    t <- compileLeftValue e
    t' <- case t of
        Type t -> return t
        Pointer (Type t) -> return t
        _ -> throw $ InvalidDotType t
    structures <- gets structures
    (_, t'') <- maybe (throw (NoMemberInType t' m)) return $ getValue m $ structures M.! t'
    return t''
compileLeftValue (Unary "*" e) = do
    t <- compileLeftValue e
    case t of
        Pointer t' -> return t'
        _ -> throw $ CannotDereference t

compileExpression :: Expression -> FunctionCompiler [Type]
compileExpression (Int i) = do
    push (imm i)
    return ["int"]
compileExpression (String "") = do
    push (imm 0)
    return ["string"]
compileExpression (String s) = do
    push =<< getStringLiteral s
    return ["string"]
compileExpression (Boolean b) = do
    push (imm (if b then 1 else 0))
    return ["bool"]
compileExpression Nil = do
    push (imm 0)
    return [NilType]
compileExpression (Variable "_") = throw Underscore
compileExpression (Variable v) = do
    t <- compileLeftValue (Variable v)
    return [t]
compileExpression (Dot e m) = do
    t <- singleType =<< compileExpression e
    t' <- case t of
        Type t -> return t
        Pointer (Type t) -> return t
        _ -> throw $ InvalidDotType t
    structures <- gets structures
    (_, t'') <- maybe (throw (NoMemberInType t' m)) return $ getValue m $ structures M.! t'
    return [t'']
compileExpression (Call "new" [Variable s]) = do
    structures <- gets structures
    if isJust (lookup s builtinTypes) || M.member s structures then
        return [Pointer (Type s)]
    else
        throw $ NoSuchStructure s
compileExpression (Call "new" es) = do
    throw WrongArgumentsForNew
compileExpression (Call fn es) = do
    Function{..} <- maybe (throw (UndefinedFunction fn)) return . M.lookup fn =<< gets functions
    let ps = map snd parameters
    ts <- mapM compileExpression es
    ts <- case ts of
        [t] | length ps > 1 -> return t
        _ -> mapM singleType ts
    unless (length ts == length ps) $ throw BadArity
    zipWithM_ matchTypes ps ts
    return returns
compileExpression (Print es) = do
    fmtImported <- gets fmtImported
    unless fmtImported $ throw FmtNotImported
    setFmtUsed
    mapM_ compileExpression es
    return []
compileExpression (Unary "!" e) = do
    t <- singleType =<< compileExpression e
    matchTypes t "bool"
    return ["bool"]
compileExpression (Unary "-" e) = do
    t <- singleType =<< compileExpression e
    matchTypes t "int"
    return ["int"]
compileExpression (Unary "*" e) = do
    t <- singleType =<< compileExpression e
    case t of
        Pointer t' -> return [t']
        _ -> throw $ CannotDereference t
compileExpression (Unary "&" e) = do
    t <- compileLeftValue e
    return [Pointer t]
compileExpression (Binary op e1 e2) | op `elem` ["==", "!="] = do
    t1 <- singleType =<< compileExpression e1
    t2 <- singleType =<< compileExpression e2
    matchTypes t1 t2
    return ["bool"]
compileExpression (Binary op e1 e2) | op `elem` ["<", "<=", ">", ">="] = do
    t1 <- singleType =<< compileExpression e1
    matchTypes t1 "int"
    t2 <- singleType =<< compileExpression e2
    matchTypes t2 "int"
    return ["bool"]
compileExpression (Binary op e1 e2) | op `elem` ["+", "-", "*", "/", "%"] = do
    t1 <- singleType =<< compileExpression e1
    matchTypes t1 "int"
    t2 <- singleType =<< compileExpression e2
    matchTypes t2 "int"
    return ["int"]
compileExpression (Binary op e1 e2) | op `elem` ["&&", "||"] = do
    t1 <- singleType =<< compileExpression e1
    matchTypes t1 "bool"
    t2 <- singleType =<< compileExpression e2
    matchTypes t2 "bool"
    return ["bool"]

singleType :: [Type] -> FunctionCompiler Type
singleType [t] = return t
singleType ts = throw $ ExpectedSingleType ts

singleConcreteType :: [Type] -> FunctionCompiler Type
singleConcreteType ts = do
    t <- singleType ts
    case t of
        NilType -> throw NoConcreteTypeForNil
        _ -> return t

matchTypes :: Type -> Type -> FunctionCompiler ()
matchTypes NilType NilType = throw CannotMatchNils
matchTypes NilType (Pointer _) = return ()
matchTypes (Pointer _) NilType = return ()
matchTypes t1 t2 = unless (t1 == t2) $ throw $ CannotMatchTypes t1 t2

-- printExpressions :: [Expression] -> FunctionCompiler ()
-- printExpressions = go False where
--     go _ [] = return ()
--     go s (e:es) = do
--         t <- head <$> compileExpression e
--         go (t /= "string") es

compileStringLiterals :: Compiler ()
compileStringLiterals = do
    strings <- gets strings
    void $ flip M.traverseWithKey strings \s l -> do
        label l
        string s

compileFile :: File -> Either TypeError String
compileFile (File fmtImported structuresList functionsList) =
    runExcept $ execWriterT $ flip evalStateT initialGlobalState do
        addStructures structuresList
        addFunctions functionsList
        text
        entryPoint
        mapM_ compileFunction functionsList
        data_
        compileStringLiterals
        functions <- gets functions
        unless ("main" `M.member` functions) $ throw NoMain
        fmtUsed <- gets fmtUsed
        when (fmtImported && not fmtUsed) $ throw FmtNotUsed
    where
        initialGlobalState = GlobalState { structures   = M.empty
                                         , functions    = M.empty
                                         , fmtImported  = fmtImported
                                         , fmtUsed      = False
                                         , labelCounter = 0
                                         , strings      = M.empty
                                         }
