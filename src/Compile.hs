{-# LANGUAGE FlexibleContexts #-}
module Compile where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Strict
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import CodeGen
import Parse (Type(..), File(File), Signature, Statement(..), Expression(..))
import qualified Parse as P (Function(..))
import Transformers

{-
effects:
    exceptions: type errors
    read-only environment:
        - structures: fields, sizes, offsets
        - functions: parameter (fields, sizes, offsets), return types (sizes, offsets)
        - current function: name or function entry
    state, per-function:
        - scopes (variables : type, offset, used?)
        - returned?
    state, global:
        - fmt used?
        - string literals
        - labels
    output:
        - assembly code
-}

type TypeError = String

data Variable = Variable { offset :: Int
                         , type_  :: Type
                         }

data Bindings = { size     :: Int
                , bindings :: String `Map` Variable
                }

emptyBindings = Bindings 0 M.empty

type Structure = Bindings

type Stuctures = String `Map` Structure

data Function = Function { parameters :: [(String, Variable)]
                         , returns    :: [Variable]
                         }

type Functions = String `Map` Function

type Environment = [Bindings]

getStructures :: MonadReader Structures m => m Structures
getStructures = ask

sizeof :: MonadReader Structures m => Type -> m Integer
sizeof (Type t) = do
    structs <- getStructures
    case M.lookup t structs of
        Just (Structure s _) -> return s
        Nothing | t `elem` ["int", "bool", "string"] -> return 8
                | otherwise -> throwError $ "unknown type " ++ t
sizeof (Pointer _) = return 8

compileGo (File importFmt structuresList functionsList) = runExcept $ execWriterT $ do
    structs <- makeStructures structuresList
    funcs <- makeFunctions functionsList
    text
    entryPoint
    mapM_ (compileFunction structs funcs) functionsList
    unless (M.member "main" funcs) $ throwError "no main function"

makeStructures structuresList = do
    let addStructure ss (n, s) = do
            when (M.member n ss) $ throwError $ "duplicate structure " ++ n
            return $ M.insert n s ss
    signatures <- foldM addStructure M.empty structuresList
    let computeSize seen n = do
            when (n `elem` seen) $ throwError $ "recursive structure" ++ (if length seen > 1 then "s" else "")  ++ " " ++ intercalate ", " (reverse seen)
            let addField (Structure o m) (f, t) = do
                    when (M.member f m) $ throwError $ "duplicate field " ++ f ++ " in structure " ++ n
                    structs <- get
                    case t of
                        Type n' | M.member n' signatures && not (M.member n' structs) -> computeSize (n:seen) n'
                        _ -> return ()
                    s <- sizeof t
                    return $ Structure (o + s) (M.insert f (o, t) m)
            struct <- foldM addField (Structure 0 M.empty) (signatures M.! n)
            modify' $ M.insert n struct
        computeSizes = mapM_ (computeSize []) (M.keysSet signatures)
    execStateT computeSizes M.empty

makeFunctions functionsList = do
    let addFunction fs (n, P.Function params returns _) = do
            when (M.member n fs) $ throwError $ "duplicate function " ++ n
            return $ M.insert n Function fs
    foldM addFunction M.empty functionsList

function = ("pgo_f_" ++)

entryPoint = do
    global "main"
    label "main"
    call (function "main")
    zero rax
    ret

compileFunction (n, P.Function params returns body) = do
    when (n == "main") $ do
        unless (null params) $ throwError "main function should not take parameters"
        unless (null returns) $ throwError "main function should not return anything"
    label (function n)
    push rbp
    mov rsp rbp
    compileBlock body
    pop rbp
    ret

compileBlock b = do
    local newScope $ mapM_ compileStatement b

compileStatement (Block b) = compileBlock b

compileStatement (Expression _) = do
    return ()
