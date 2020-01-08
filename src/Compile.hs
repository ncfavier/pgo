{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (compileFile, TypeError(..)) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe

import AST hiding (structures, functions)
import Pack (Pack)
import qualified Pack as P
import X86_64

-- | Cette instance permet d'accéder à différentes couches de `StateT` sans utiliser `lift`,
-- pourvu que les types d'états soient différents.
-- J'en ai besoin pour pouvoir accéder à l'état global de la même manière dans `Compiler` et dans `FunctionCompiler`.
instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (StateT s' m) where
    get = lift get
    put = lift . put

type TypeError = Located String

throw e = throwError (e :@ nowhere)
throwAt l e = throwError (e :@ l)
plural 1 thing = thing
plural n thing = thing ++ "s"
nThings 0 thing = "no " ++ thing ++ "s"
nThings n thing = show n ++ " " ++ plural n thing

data GlobalState = GlobalState { structures         :: Map String (Maybe Structure)
                               , functions          :: Map String Function
                               , fmtImported        :: Bool
                               , fmtUsed            :: Bool
                               , labelCounter       :: Integer
                               , stringLabelCounter :: Integer
                               , strings            :: Map String String
                               }

type Structure = Pack Type

data FunctionState = FunctionState { currentFunction :: (Identifier, Function)
                                   , scopes          :: [Scope]
                                   , returned        :: Bool
                                   , returnLabel     :: String
                                   , returnPack      :: Pack Type
                                   }

data LocalVar = LocalVar { varType   :: Type
                         , used      :: Bool
                         , definedAt :: Location
                         }

type Scope = Pack LocalVar

type Compiler = StateT GlobalState (WriterT String (Except TypeError))

type FunctionCompiler = StateT FunctionState Compiler

getStructure s = fromJust . (M.! s) <$> gets structures
getMember s m = P.getOffsetAndValueByName m <$> getStructure s

getFunction f = M.lookup f <$> gets functions

setFmtUsed = modify' $ \gs -> gs { fmtUsed = True }

getFreshLabel = do
    c <- gets labelCounter
    modify' $ \gs -> gs { labelCounter = succ c }
    return $ ".L" ++ show c

getStringLiteral "" = return (imm 0)
getStringLiteral s = immLabel <$> do
    strings <- gets strings
    case M.lookup s strings of
        Just l -> return l
        Nothing -> do
            c <- gets stringLabelCounter
            modify' $ \gs -> gs { stringLabelCounter = succ c }
            let l = ".LS" ++ show c
            modify' $ \gs -> gs { strings = M.insert s l strings }
            return l

getBottom = gets (P.bottom . head . scopes)

pushScope s = modify' $ \fs -> fs { scopes = s:scopes fs }
popScope = modify' $ \fs -> fs { scopes = tail (scopes fs) }

getLocalVar :: Identifier -> FunctionCompiler (Integer, LocalVar)
getLocalVar (v :@ l) = do
    scopes <- gets scopes
    let (m, scopes') = mapAccumL f Nothing scopes
    modify' $ \fs -> fs { scopes = scopes' }
    maybe (throwAt l $ "unbound variable " ++ v) return m
    where
        f Nothing s | Just (o, lv) <- P.getOffsetAndValueByName v s =
                        (Just (o, lv), P.updateByName v lv { used = True } s)
                    | otherwise = (Nothing, s)
        f m s = (m, s)

addLocalVar :: Type -> Identifier -> FunctionCompiler ()
addLocalVar t (v :@ l) = do
    s <- sizeof t
    ~(sc:scs) <- gets scopes
    sc' <- case v of
        "_" -> return $ P.pushDown s (LocalVar t True l) sc
        _ | Just sc' <- P.pushDownWithName v s (LocalVar t False l) sc -> return sc'
          | otherwise                                                  -> throwAt l $ "redefined variable " ++ v
    modify' $ \fs -> fs { scopes = sc':scs }

setReturned r = modify' $ \fs -> fs { returned = r }

builtinTypes = [("int", 8), ("bool", 8), ("string", 8)]

sizeof (Type (t :@ l)) = do
    structures <- gets structures
    if | Just s <- M.lookup t structures -> return (maybe 0 P.size s)
       | Just s <- lookup t builtinTypes -> return s
       | otherwise                       -> throwAt l $ "invalid type " ++ t
sizeof (Pointer t) = 8 <$ sizeof t
sizeof NilType = return 8

packTypesUpwards offset = foldrM f (P.emptyPackAt offset)
    where f t p = do s <- sizeof t; return (P.pushUp s t p)

packTypesDownwards offset = foldlM f (P.emptyPackAt offset)
    where f p t = do s <- sizeof t; return (P.pushDown s t p)

addStructures :: [(Identifier, Fields)] -> Compiler ()
addStructures structuresList = do
    let addRawStructure ss (n :@ l, s) = do
            when (n `M.member` ss) $ throwAt l $ "duplicate structure " ++ n
            modify' $ \gs -> gs { structures = M.insert n Nothing (structures gs) }
            return $ M.insert n (s :@ l) ss
    rawStructures <- foldlM addRawStructure M.empty structuresList
    let addStructure = go [] where
            go seen n = do
                let rawStruct :@ l = rawStructures M.! n
                when (n `elem` seen) $ throwAt l $ "recursive " ++ plural (length seen) "structure" ++ " " ++ intercalate ", " seen
                let addField s (f :@ lf, t) = do
                        structures <- gets structures
                        case t of
                            Type (n' :@ _) | n' `M.member` rawStructures, Nothing <- structures M.! n' -> go (n:seen) n'
                            _ -> return ()
                        size <- sizeof t
                        maybe (throwAt lf $ "duplicate field " ++ f ++ " in structure " ++ n) return $ P.pushUpWithName f size t s
                structure <- foldlM addField P.emptyPack rawStruct
                modify' $ \gs -> gs { structures = M.insert n (Just structure) (structures gs) }
    mapM_ addStructure (M.keysSet rawStructures)

addFunctions :: [(Identifier, Function)] -> Compiler ()
addFunctions = mapM_ $ \(n :@ l, f) -> do
    functions <- gets functions
    when (n `M.member` functions) $ throwAt l $ "duplicate function " ++ n
    modify' $ \gs -> gs { functions = M.insert n f functions }

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
    when (n == "main") $ do
        unless (null parameters) $ throwAt l "main should not take any parameters"
        unless (null returns)    $ throwAt l "main should not return anything"
    label (function n)
    push rbp
    mov rsp rbp
    let addParameter (v :@ l, t) s = do
            s' <- sizeof t
            maybe (throwAt l $ "duplicate parameter " ++ v ++ " in function " ++ n) return $ P.pushUpWithName v s' (LocalVar t True nowhere) s
    paramsScope <- foldrM addParameter (P.emptyPackAt 16) parameters
    returnPack <- packTypesUpwards (P.top paramsScope) returns
    returnLabel <- getFreshLabel
    let paramsSize = P.size paramsScope
        initialFunctionState = FunctionState { currentFunction = f
                                             , scopes = [paramsScope { P.bottom = 0 }]
                                             , returned = False
                                             , returnLabel
                                             , returnPack
                                             }
    FunctionState{..} <- execStateT (mapM_ compileStatement body >> checkUnused) initialFunctionState
    when (length returns > 0 && not returned) $ throwAt l $ "missing return statement in function " ++ n
    label returnLabel
    leave
    ret1 (imm paramsSize)

checkUnused :: FunctionCompiler ()
checkUnused = do
    vars <- gets (P.nameAssocs . head . scopes)
    void $ traverse f vars
    where
        f (v, LocalVar { used = False, definedAt = l }) = throwAt l $ "unused variable " ++ v
        f _ = return ()

moveQuad :: Relative -> Relative -> FunctionCompiler ()
moveQuad (so `Rel` sb) (to `Rel` tb) = do
    mov (so `rel` sb) rax
    mov rax (to `rel` tb)

move :: Type -> Relative -> Relative -> FunctionCompiler ()
move (Type ("int" :@ _)) = moveQuad
move (Type ("bool" :@ _)) = moveQuad
move (Type ("string" :@ _)) = moveQuad
move (Pointer _) = moveQuad
move NilType = moveQuad
move (Type (s :@ _)) = \(so `Rel` sb) (to `Rel` tb) -> do
    struct <- getStructure s
    forM_ (P.upwardsAssocs struct) $ \(o, t) -> move t (so + o `Rel` sb) (to + o `Rel` tb)

printCharacter c = do
    mov (imm (ord c)) rdi
    call "putchar"

printNil = do
    fmt <- getStringLiteral "<nil>"
    mov fmt rdi
    zero rax
    call "printf"

printExpressions :: Bool -> String -> Pack Type -> FunctionCompiler ()
printExpressions alwaysSpace base p = do
    void $ runStateT (P.traverseDownwards (printExpression alwaysSpace base) p) False

printExpression :: Bool -> String -> Integer -> Type -> StateT Bool FunctionCompiler ()
printExpression alwaysSpace base o t = do
    space <- get
    case t of
        Type ("string" :@ _) -> put alwaysSpace
        _ -> do
            when space (printCharacter ' ')
            put True
    case t of
        Type ("int" :@ _) -> do
            fmt <- getStringLiteral "%lld"
            mov fmt rdi
            mov (o `rel` base) rsi
            zero rax
            call "printf"
        Type ("bool" :@ _) -> do
            falseFmt <- getStringLiteral "false"
            trueFmt <- getStringLiteral "true"
            mov falseFmt rax
            mov trueFmt rbx
            cmp (imm 0) (o `rel` base)
            cmove rax rdi
            cmovne rbx rdi
            zero rax
            call "printf"
        Type ("string" :@ _) -> do
            mov (o `rel` base) rax
            cmp (imm 0) rax
            je "0f"
            fmt <- getStringLiteral "%s"
            mov fmt rdi
            mov rax rsi
            zero rax
            call "printf"
            label "0"
        Type (s :@ _) -> do
            fmt <- getStringLiteral $ "{struct " ++ s ++ "}"
            mov fmt rdi
            zero rax
            call "printf"
            -- printCharacter '{'
            -- struct <- getStructure s
            -- lift $ printExpressions True base struct
            -- printCharacter '}'
        NilType -> printNil
        Pointer _ -> do
            mov (o `rel` base) rax
            cmp (imm 0) rax
            je "0f"
            fmt <- getStringLiteral "%p"
            mov fmt rdi
            mov rax rsi
            zero rax
            call "printf"
            jmp "1f"
            label "0"
            printNil
            label "1"

compileBlock :: [Statement] -> FunctionCompiler ()
compileBlock b = do
    bottom <- getBottom
    pushScope (P.emptyPackAt bottom)
    mapM_ compileStatement b
    checkUnused
    popScope
    lea (bottom `rel` rbp) rsp

compileStatement :: Statement -> FunctionCompiler ()
compileStatement (Block b) = compileBlock b
compileStatement (Expression e) = do
    compileExpression e
    bottom <- getBottom
    lea (bottom `rel` rbp) rsp
compileStatement (Increment e) = do
    compileLeftValueAs "int" e
    pop rax
    mov (0 `rel` rax) rbx
    inc rbx
    mov rbx (0 `rel` rax)
compileStatement (Decrement e) = do
    compileLeftValueAs "int" e
    pop rax
    mov (0 `rel` rax) rbx
    dec rbx
    mov rbx (0 `rel` rax)
compileStatement (Var vs (Just t) ([] :@ _)) = do
    s <- sizeof t
    let s' = s * genericLength vs
    sub (imm s') rsp
    mov rsp rdi
    zero rsi
    mov (imm s') rdx
    call "memset"
    mapM_ (addLocalVar t) vs
compileStatement (Var vs (Just t) es) = do
    compileExpressionsAs (t <$ vs) es
    mapM_ (addLocalVar t) vs
compileStatement (Var vs Nothing es@(_ :@ l)) = do
    ts <- compileExpressionsWith compileConcreteExpression es
    unless (length ts == length vs) $ throwAt l $ "expected " ++ nThings (length vs) "value" ++ ", got " ++ nThings (length ts) "value"
    zipWithM_ addLocalVar ts vs
compileStatement (Assign vs es) = do
    bottom <- getBottom
    ts <- mapM compileLeftValue vs
    pv <- packTypesDownwards bottom (NilType <$ vs)
    compileExpressionsAs ts es
    pe <- packTypesDownwards (P.bottom pv) ts
    let f (vo, _) (eo, t) = do
            mov (vo `rel` rbp) rbx
            move t (eo `Rel` rbp) (0 `Rel` rbx)
    zipWithM f (M.assocs (P.objects pv)) (M.assocs (P.objects pe))
    lea (bottom `rel` rbp) rsp
compileStatement (Return es) = do
    (f :@ _, Function{..}) <- gets currentFunction
    bottom <- getBottom
    compileExpressionsAs returns es
    pe <- packTypesDownwards bottom returns
    pr <- gets returnPack
    let f (ro, _) (eo, t) = do
            move t (eo `Rel` rbp) (ro `Rel` rbp)
    zipWithM f (M.assocs (P.objects pr)) (M.assocs (P.objects pe))
    returnLabel <- gets returnLabel
    jmp returnLabel
    setReturned True
compileStatement (If cond yes no) = do
    compileExpressionAs "bool" cond
    pop rax
    cmp (imm 0) rax
    labelFalse <- getFreshLabel
    labelEnd <- getFreshLabel
    je labelFalse
    r0 <- gets returned
    compileBlock yes
    r1 <- gets returned
    setReturned r0
    jmp labelEnd
    label labelFalse
    compileBlock no
    r2 <- gets returned
    setReturned (r1 && r2)
    label labelEnd
compileStatement (For cond b) = do
    labelBody <- getFreshLabel
    labelCond <- getFreshLabel
    jmp labelCond
    label labelBody
    r0 <- gets returned
    compileBlock b
    setReturned r0
    label labelCond
    compileExpressionAs "bool" cond
    pop rax
    cmp (imm 0) rax
    jne labelBody

compileLeftValue :: Expression -> FunctionCompiler Type
compileLeftValue (Variable v :@ _) = do
    (o, LocalVar{..}) <- getLocalVar v
    lea (o `rel` rbp) rax
    push rax
    return varType
compileLeftValue (Dot (e :@ le) (m :@ lm) :@ _) = do
    t <- compileLeftValue (e :@ le)
    pop rax
    s :@ _ <- case t of
        Type t -> return t
        Pointer (Type t) -> do
            mov (0 `rel` rax) rax
            return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (o, t') <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return =<< getMember s m
    add (imm o) rax
    push rax
    return t'
compileLeftValue (Unary ("*" :@ _) (e :@ l) :@ _) = do
    t <- compileLeftValue (e :@ l)
    case t of
        Pointer t' -> do
            pop rax
            push (0 `rel` rax)
            return t'
        _ -> throwAt l $ "cannot dereference a value of type " ++ show t
compileLeftValue (e :@ l) = throwAt l "this expression is not a left value"

compileLeftValueAs :: Type -> Expression -> FunctionCompiler ()
compileLeftValueAs t e@(_ :@ l) = do
    t' <- compileLeftValue e
    unless (t `matches` t') $ throwAt l $ "this value has type " ++ show t' ++ " but was expected of type " ++ show t

compileExpression :: Expression -> FunctionCompiler [Type]
compileExpression (Int i :@ _) = do
    mov (imm i) rax
    push rax
    return ["int"]
compileExpression (String s :@ _) = do
    l <- getStringLiteral s
    push l
    return ["string"]
compileExpression (Bool b :@ _) = do
    push (imm (if b then 1 else 0))
    return ["bool"]
compileExpression (Nil :@ _) = do
    push (imm 0)
    return [NilType]
compileExpression (Variable ("_" :@ l) :@ _) = throwAt l "the variable _ has no value"
compileExpression (Variable v :@ _) = do
    (o, LocalVar{..}) <- getLocalVar v
    s <- sizeof varType
    sub (imm s) rsp
    move varType (o `Rel` rbp) (0 `Rel` rsp)
    return [varType]
compileExpression (Dot e@(_ :@ le) (m :@ lm) :@ _) = do
    t <- compileSimpleExpression e
    st <- sizeof t
    s :@ _ <- case t of
        Type t -> do
            mov rsp rbx
            add (imm st) rsp
            return t
        Pointer (Type t) -> do
            pop rbx
            return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (o, tm) <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return =<< getMember s m
    stm <- sizeof tm
    sub (imm stm) rsp
    move tm (o `Rel` rbx) (0 `Rel` rsp)
    return [tm]
compileExpression (Call ("new" :@ _) ([Variable n :@ _] :@ _) :@ _) = do
    let t = Type n
    s <- sizeof t
    mov (imm 1) rdi
    mov (imm s) rsi
    call "calloc"
    push rax
    return [Pointer t]
compileExpression (Call ("new" :@ _) (_ :@ l) :@ _) = do
    throwAt l "new() expects a single type name"
compileExpression (Call (f :@ l) es :@ _) = do
    Function{..} <- maybe (throwAt l $ "undefined function " ++ f) return =<< getFunction f
    let ts = map snd parameters
    p <- packTypesDownwards 0 returns
    sub (imm (P.size p)) rsp
    compileExpressionsAs ts es
    call (function f)
    return returns
compileExpression (Print es :@ l) = do
    fmtImported <- gets fmtImported
    unless fmtImported $ throwAt l "fmt used but not imported"
    setFmtUsed
    ts <- compileExpressionsWith compileSimpleExpression es
    p <- packTypesUpwards 0 ts
    printExpressions False rsp p
    add (imm (P.size p)) rsp
    return []
compileExpression (Unary ("!" :@ _) e :@ _) = do
    compileExpressionAs "bool" e
    pop rax
    xor (imm 1) rax
    push rax
    return ["bool"]
compileExpression (Unary ("-" :@ _) e :@ _) = do
    compileExpressionAs "int" e
    pop rax
    neg rax
    push rax
    return ["int"]
compileExpression (Unary ("*" :@ _) e@(_ :@ l) :@ _) = do
    t <- compileSimpleExpression e
    case t of
        Pointer t' -> do
            pop rbx
            s <- sizeof t'
            sub (imm s) rsp
            move t' (0 `Rel` rbx) (0 `Rel` rsp)
            return [t']
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
    pop rbx
    pop rax
    cmp rbx rax
    al & case op of
        "==" -> sete
        "!=" -> setne
    movzbq al rax
    push rax
    -- TODO: compare type t1/t2 between o1(rbp) and o2(rbp)
    return ["bool"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["<", "<=", ">", ">="] = do
    compileExpressionAs "int" e1
    compileExpressionAs "int" e2
    pop rbx
    pop rax
    cmp rbx rax
    al & case op of
        "<" -> setl
        "<=" -> setle
        ">" -> setg
        ">=" -> setge
    movzbq al rax
    push rax
    return ["bool"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["+", "-", "*", "/", "%"] = do
    compileExpressionAs "int" e1
    compileExpressionAs "int" e2
    pop rbx
    pop rax
    case op of
        "+" -> add rbx rax
        "-" -> sub rbx rax
        "*" -> imul rbx rax
        "/" -> cqto >> idiv rbx
        "%" -> cqto >> idiv rbx >> mov rdx rax
    push rax
    return ["int"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["&&", "||"] = do
    compileExpressionAs "bool" e1
    compileExpressionAs "bool" e2
    pop rbx
    pop rax
    let test = case op of
            "&&" -> and'
            "||" -> or'
    test rbx rax
    push rax
    return ["bool"]
compileExpression (Binary (op :@ l) _ _ :@ _) = throwAt l $ "unknown binary operator " ++ op

-- Compile une seule expression simple, c'est à dire avec exactement un type.
compileSimpleExpression :: Expression -> FunctionCompiler Type
compileSimpleExpression e@(_ :@ l) = do
    ts <- compileExpression e
    case ts of
        [t] -> return t
        [] -> throwAt l $ "this expression has no value but a single value was expected"
        _ -> throwAt l $ "this expression returns " ++ show (length ts) ++ " values but a single value was expected"

-- Compile une seule expression concrète, c'est à dire une expression simple différente de nil.
compileConcreteExpression :: Expression -> FunctionCompiler Type
compileConcreteExpression (Nil :@ l) = throwAt l "nil has no concrete type"
compileConcreteExpression e = compileSimpleExpression e

-- Compile une expression avec un type donné.
compileExpressionAs :: Type -> Expression -> FunctionCompiler ()
compileExpressionAs t e@(_ :@ l) = do
    t' <- compileSimpleExpression e
    unless (t' `matches` t) $ throwAt l $ "this expression has type " ++ show t' ++ " but was expected of type " ++ show t

-- Compile une ou plusieurs expressions.
compileExpressionsWith :: (Expression -> FunctionCompiler Type) -> Expressions -> FunctionCompiler [Type]
compileExpressionsWith _ ([e@(Call _ _ :@ _)] :@ _) = compileExpression e
compileExpressionsWith f (es :@ l) = mapM f es

-- Compile une ou plusieurs expressions avec des types donnés.
compileExpressionsAs :: [Type] -> Expressions -> FunctionCompiler ()
compileExpressionsAs ts ([e@(Call _ _ :@ l)] :@ _) | length ts > 1 = do
    ts' <- compileExpression e
    unless (length ts' == length ts) $ throwAt l $ "this function returns " ++ nThings (length ts') "value" ++ " but " ++ nThings (length ts) "value" ++ " were expected"
    unless (and $ zipWith matches ts ts') $ throwAt l $ "couldn't match type " ++ show ts' ++ " with expected type " ++ show ts
compileExpressionsAs ts (es :@ l) = do
    unless (length es == length ts) $ throwAt l $ "expected " ++ nThings (length ts) "expression" ++ ", got " ++ show (length es)
    zipWithM_ compileExpressionAs ts es

matches :: Type -> Type -> Bool
NilType   `matches` NilType   = False
NilType   `matches` Pointer _ = True
Pointer _ `matches` NilType   = True
a         `matches` b         = a == b

compileStringLiterals :: Compiler ()
compileStringLiterals = do
    strings <- gets strings
    void $ flip M.traverseWithKey strings $ \s l -> do
        label l
        string s

compileFile :: File -> Either TypeError String
compileFile (File importFmt structuresList functionsList) =
    runExcept $ execWriterT $ flip evalStateT initialGlobalState $ do
        addStructures structuresList
        addFunctions functionsList
        text
        entryPoint
        mapM_ compileFunction functionsList
        data'
        compileStringLiterals
        functions <- gets functions
        unless ("main" `M.member` functions) $ throw "no main function"
        fmtUsed <- gets fmtUsed
        case importFmt of
            Just (() :@ l) | not fmtUsed -> throwAt l "fmt imported but not used"
            _ -> return ()
    where
        initialGlobalState = GlobalState { structures         = M.empty
                                         , functions          = M.empty
                                         , fmtImported        = isJust importFmt
                                         , fmtUsed            = False
                                         , labelCounter       = 0
                                         , stringLabelCounter = 0
                                         , strings            = M.empty
                                         }
