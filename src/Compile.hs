{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Compile (compileFile) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State.Strict
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

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
               | UnboundVariable String
               | UndefinedFunction String
               | BadArity
               | WrongType Type [Type]
               | WrongTypeForNil Type
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

getStringLiteral s = do
    strings <- gets strings
    case M.lookup s strings of
        Just l -> return l
        Nothing -> do
            c <- gets labelCounter
            let l = ".LS" ++ show c
            modify' \gs -> gs { strings = M.insert s l strings }
            return l

getBottom = gets (bottom . head . scopes)

addScope s = modify' \fs -> fs { scopes = s:scopes fs }

sizeof :: (MonadState GlobalState m, MonadError TypeError m) => Type -> m Integer
sizeof (Type t) = do
    structures <- gets structures
    if | Just s <- M.lookup t structures -> return (size s)
       | Just s <- lookup t builtinTypes -> return s
       | otherwise                       -> throw (InvalidType t)
    where builtinTypes = [("int", 8), ("bool", 8), ("string", 8)]
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
    runStateT (compileBlock body) initialFunctionState
    leave
    ret1 (imm paramsSize)

compileBlock :: [Statement] -> FunctionCompiler ()
compileBlock = mapM_ compileStatement

compileStatement :: Statement -> FunctionCompiler ()
compileStatement (Block b) = do
    bottom <- getBottom
    addScope (emptyPackOffset bottom)
    compileBlock b
compileStatement (Expression e) = do
    compileExpression e
    bottom <- getBottom
    lea (bottom `rel` rbp) rsp
compileStatement (Increment e) = do
    return ()
compileStatement (Decrement e) = do
    return ()
compileStatement (Var vs (Just t) es) = do
    return ()
compileStatement (Var vs Nothing es) = do
    return ()
compileStatement (Assign vs es) = do
    return ()
compileStatement (Return es) = do
    return ()
compileStatement (If cond yes no) = do
    return ()
compileStatement (For cond b) = do
    return ()

compileExpression :: Expression -> FunctionCompiler [Type]
compileExpression (Int i) = do
    push (imm i)
    return [Type "int"]
compileExpression (String s) = do
    l <- getStringLiteral s
    push (immLabel l)
    return [Type "string"]
compileExpression (Boolean b) = do
    push (imm (if b then 1 else 0))
    return [Type "bool"]
compileExpression Nil = do
    push (imm 0)
    return []
compileExpression (Variable v) = do
    scopes <- gets scopes
    lv <- maybe (throw (UnboundVariable v)) return $ asum $ getValue v <$> scopes
    return [varType lv]
compileExpression (Dot e m) = do
    return []
compileExpression (Call fn es) = do
    Function{..} <- maybe (throw (UndefinedFunction fn)) return . M.lookup fn =<< gets functions
    when (length parameters /= length es) $ throw BadArity
    return returns
compileExpression (Print es) = do
    fmtImported <- gets fmtImported
    unless fmtImported $ throw FmtNotImported
    setFmtUsed
    printExpressions es
    return []
compileExpression (Unary op e) = do
    return []
compileExpression (Binary op e1 e2) = do
    return []

compileExpressionAsType :: Type -> Expression -> FunctionCompiler ()
compileExpressionAsType (Pointer _) Nil = do
    void $ compileExpression Nil
compileExpressionAsType t Nil = do
    throwError $ WrongTypeForNil t
compileExpressionAsType t e = do
    t' <- compileExpression e
    unless (t' == [t]) $ throwError $ WrongType t t'

printExpressions :: [Expression] -> FunctionCompiler ()
printExpressions = go False where
    go _ [] = return ()
    go s (e:es) = do
        t <- head <$> compileExpression e
        go (t /= Type "string") es

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
