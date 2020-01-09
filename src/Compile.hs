{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
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

import AST hiding (structures, functions, Function(..))
import qualified AST
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

data Function = Function { functionName   :: Identifier
                         , parameters     :: Fields
                         , returns        :: [Type]
                         , body           :: [Statement]
                         , parametersPack :: Scope
                         , returnPack     :: Pack Type
                         }

data FunctionState = FunctionState { currentFunction :: Function
                                   , scopes          :: [Scope]
                                   , returned        :: Bool
                                   , returnLabel     :: String
                                   }

data LocalVar = LocalVar { varType         :: Type
                         , used            :: Bool
                         , definedAt       :: Location
                         , allocatedOnHeap :: Bool
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

getLocalVar :: Bool -> Identifier -> FunctionCompiler (Integer, LocalVar)
getLocalVar used (v :@ l) = do
    scopes <- gets scopes
    let (m, scopes') = mapAccumL f Nothing scopes
    modify' $ \fs -> fs { scopes = scopes' }
    maybe (throwAt l $ "unbound variable " ++ v) return m
    where
        f Nothing s | Just (o, lv) <- P.getOffsetAndValueByName v s =
                        (Just (o, lv), P.updateByName v lv { used } s)
                    | otherwise = (Nothing, s)
        f m s = (m, s)

setReturned r = modify' $ \fs -> fs { returned = r }

sizeof (Type ("int" :@ _)) = return 8
sizeof (Type ("bool" :@ _)) = return 8
sizeof (Type ("string" :@ _)) = return 8
sizeof (Type (t :@ l)) = do
    structures <- gets structures
    case M.lookup t structures of
        Just s -> return (maybe 0 P.size s)
        _      -> throwAt l $ "invalid type " ++ t
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

addFunctions :: [(Identifier, AST.Function)] -> Compiler ()
addFunctions = mapM_ $ \(n :@ l, AST.Function parameters returns body) -> do
    when (n == "main") $ do
        unless (null parameters) $ throwAt l "main should not take any parameters"
        unless (null returns)    $ throwAt l "main should not return anything"
    let addParameter (v :@ l, t) sc = do
            s <- sizeof t
            let allocatedOnHeap = shouldAllocateOnHeap v body
                lv = LocalVar { varType   = t
                              , used      = True
                              , definedAt = nowhere
                              , allocatedOnHeap
                              }
                s' | allocatedOnHeap = 8
                   | otherwise       = s
            maybe (throwAt l $ "duplicate parameter " ++ v ++ " in function " ++ n) return $
                P.pushUpWithName v s' lv sc
    parametersPack <- foldrM addParameter (P.emptyPackAt 16) parameters
    returnPack <- packTypesUpwards (P.top parametersPack) returns
    let f = Function { functionName = n :@ l, .. }
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

compileFunction :: Function -> Compiler ()
compileFunction f@Function{..} = do
    let n :@ l = functionName
    label (function n)
    push rbp
    mov rsp rbp
    returnLabel <- getFreshLabel
    let paramsSize = P.size parametersPack
        initialFunctionState = FunctionState { currentFunction = f
                                             , scopes = [parametersPack { P.bottom = 0 }]
                                             , returned = False
                                             , returnLabel
                                             }
    FunctionState{..} <- execStateT (compileStatements body) initialFunctionState
    when (length returns > 0 && not returned) $ throwAt l $ "missing return statement in function " ++ n
    label returnLabel
    leave
    ret1 (imm paramsSize)

checkUnused :: FunctionCompiler ()
checkUnused = do
    vars <- gets (P.nameAssocs . head . scopes)
    sequence_ [ throwAt l $ "unused variable " ++ v
              | (v, LocalVar { used = False, definedAt = l }) <- vars ]

compQuad :: Relative -> Relative -> FunctionCompiler ()
compQuad (ao `Rel` ab) (bo `Rel` bb) = do
    mov (ao `rel` ab) rax
    cmp (bo `rel` bb) rax
    jne "0f"

comp :: Type -> Relative -> Relative -> FunctionCompiler ()
comp (Type ("int" :@ _)) = compQuad
comp (Type ("bool" :@ _)) = compQuad
comp (Type ("string" :@ _)) = compQuad
comp (Type (s :@ _)) = \(ao `Rel` ab) (bo `Rel` bb) -> do
    struct <- getStructure s
    forM_ (P.upwardsAssocs struct) $ \(o, t) ->
        comp t (ao + o `Rel` ab) (bo + o `Rel` bb)
comp (Pointer _) = compQuad
comp NilType = compQuad

moveQuad :: Relative -> Relative -> FunctionCompiler ()
moveQuad (so `Rel` sb) (to `Rel` tb) = do
    mov (so `rel` sb) rax
    mov rax (to `rel` tb)

move :: Type -> Relative -> Relative -> FunctionCompiler ()
move (Type ("int" :@ _)) = moveQuad
move (Type ("bool" :@ _)) = moveQuad
move (Type ("string" :@ _)) = moveQuad
move (Type (s :@ _)) = \(so `Rel` sb) (to `Rel` tb) -> do
    struct <- getStructure s
    forM_ (P.upwardsAssocs struct) $ \(o, t) ->
        move t (so + o `Rel` sb) (to + o `Rel` tb)
move (Pointer _) = moveQuad
move NilType = moveQuad

moveZeroQuad :: Relative -> FunctionCompiler ()
moveZeroQuad (to `Rel` tb) = do
    movq (imm 0) (to `rel` tb)

moveZero :: Type -> Relative -> FunctionCompiler ()
moveZero (Type ("int" :@ _)) = moveZeroQuad
moveZero (Type ("bool" :@ _)) = moveZeroQuad
moveZero (Type ("string" :@ _)) = moveZeroQuad
moveZero (Type (s :@ _)) = \(to `Rel` tb) -> do
    struct <- getStructure s
    forM_ (P.upwardsAssocs struct) $ \(o, t) ->
        moveZero t (to + o `Rel` tb)
moveZero (Pointer _) = moveZeroQuad
moveZero NilType = moveZeroQuad

printCharacter c = do
    mov (imm (ord c)) rdi
    call "putchar"

printNil = do
    fmt <- getStringLiteral "<nil>"
    mov fmt rdi
    zero rax
    call "printf"

printExpressions :: Bool -> Relative -> [(Integer, Type)] -> FunctionCompiler ()
printExpressions alwaysSpace (o `Rel` base) es = do
    flip evalStateT False $ forM_ es $ \(o', t) ->
        printExpression alwaysSpace (o + o' `Rel` base) t

printExpression :: Bool -> Relative -> Type -> StateT Bool FunctionCompiler ()
printExpression alwaysSpace (o `Rel` base) t = do
    space <- get
    case t of
        Type ("string" :@ _) -> do
            when alwaysSpace (printCharacter ' ')
            put alwaysSpace
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
            false <- getStringLiteral "false"
            true <- getStringLiteral "true"
            mov false rax
            mov true rbx
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
            printCharacter '{'
            struct <- getStructure s
            lift $ printExpressions True (o `Rel` base) (P.upwardsAssocs struct)
            printCharacter '}'
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

shouldAllocateOnHeap :: String -> [Statement] -> Bool
shouldAllocateOnHeap v b = any f b
    where
        f (Expression e) = g e
        f (Var _ _ (es :@ _)) = any g es
        f (Assign _ (es :@ _)) = any g es
        f (Return (es :@ _)) = any g es
        f (Block b) = any f b
        f (If cond yes no) = g cond || any f yes || any f no
        f (For cond body) = g cond || any f body
        f _ = False
        g (Dot e _ :@ _) = g e
        g (Call _ (es :@ _) :@ _) = any g es
        g (Print (es :@ _) :@ _) = any g es
        g (Unary ("&" :@ _) e :@ _) = h e
        g (Unary _ e :@ _) = g e
        g (Binary _ e1 e2 :@ _) = g e1 || g e2
        g _ = False
        h (Variable (v' :@ _) :@ _) = v == v'
        h (Dot e _ :@ _) = h e
        h _ = False

allocateVar :: LocalVar -> FunctionCompiler Integer
allocateVar LocalVar{..} = do
    s <- sizeof varType
    if allocatedOnHeap then do
        mov (imm 1) rdi
        mov (imm s) rsi
        call "calloc"
        push rax
        return 8
    else do
        sub (imm s) rsp
        moveZero varType (0 `Rel` rsp)
        return s

compileBlock :: [Statement] -> FunctionCompiler ()
compileBlock b = do
    bottom <- getBottom
    pushScope (P.emptyPackAt bottom)
    compileStatements b
    popScope
    lea (bottom `rel` rbp) rsp

compileStatements :: [Statement] -> FunctionCompiler ()
compileStatements (s:b) = compileStatement s b >> compileStatements b
compileStatements []    = checkUnused

compileStatement :: Statement -> [Statement] -> FunctionCompiler ()
compileStatement (Block b) _ = compileBlock b
compileStatement (Expression e) _ = do
    compileExpression e
    bottom <- getBottom
    lea (bottom `rel` rbp) rsp
compileStatement (Increment e) _ = do
    compileLeftValueAs "int" e
    pop rbx
    mov (0 `rel` rbx) rax
    inc rax
    mov rax (0 `rel` rbx)
compileStatement (Decrement e) _ = do
    compileLeftValueAs "int" e
    pop rbx
    mov (0 `rel` rbx) rax
    dec rax
    mov rax (0 `rel` rbx)
compileStatement (Var vs mt es@(_ :@ l)) b = do
    (ts, compiledExpressions) <- censor (const "") $ listen $ case mt of
        Just t -> (t <$ vs) <$ case es of
            [] :@ _ -> return ()
            _ -> compileExpressionsAs (t <$ vs) es
        Nothing -> compileExpressionsWith compileConcreteExpression es
    unless (length ts == length vs) $ throwAt l $ "expected " ++ nThings (length vs) "value" ++ ", got " ++ nThings (length ts) "value"
    forM_ (zip vs ts) $ \(v :@ l, t) -> case v of
        "_" -> return ()
        _ -> do
            let allocatedOnHeap = shouldAllocateOnHeap v b
                lv = LocalVar { varType   = t
                            , used      = False
                            , definedAt = l
                            , allocatedOnHeap
                            }
            s <- allocateVar lv
            ~(sc:scs) <- gets scopes
            sc' <- maybe (throwAt l $ "redefined variable " ++ v) return $
                P.pushDownWithName v s lv sc
            modify' $ \fs -> fs { scopes = sc':scs }
    unless (null compiledExpressions) $ do
        tell compiledExpressions
        bottom <- getBottom
        p <- packTypesDownwards bottom ts
        forM_ (zip vs (P.downwardsAssocs p)) $ \(v, (eo, t)) -> case v of
            "_" :@ _ -> return ()
            _ -> do
                (vo, LocalVar{..}) <- getLocalVar False v
                if allocatedOnHeap then do
                    mov (vo `rel` rbp) rbx
                    move t (eo `Rel` rbp) (0 `Rel` rbx)
                else do
                    move t (eo `Rel` rbp) (vo `Rel` rbp)
        lea (bottom `rel` rbp) rsp
compileStatement (Assign vs es) _ = do
    bottom <- getBottom
    ts <- compileExpressionsWith compileSimpleExpression es
    pe <- packTypesDownwards bottom ts
    forM_ (zip vs (P.downwardsAssocs pe)) $ \(v, (o, t)) -> case v of
        Variable ("_" :@ _) :@ _ -> return ()
        _ -> do
            compileLeftValueAs t v
            pop rbx
            move t (o `Rel` rbp) (0 `Rel` rbx)
    lea (bottom `rel` rbp) rsp
compileStatement (Return es) _ = do
    Function{..} <- gets currentFunction
    bottom <- getBottom
    compileExpressionsAs returns es
    pe <- packTypesDownwards bottom returns
    let f (ro, _) (eo, t) = do
            move t (eo `Rel` rbp) (ro `Rel` rbp)
    zipWithM f (P.downwardsAssocs returnPack) (P.downwardsAssocs pe)
    returnLabel <- gets returnLabel
    jmp returnLabel
    setReturned True
compileStatement (If cond yes no) _ = do
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
compileStatement (For cond b) _ = do
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
compileLeftValue (Variable ("_" :@ l) :@ _) = throwAt l "invalid use of _ as a left-value"
compileLeftValue (Variable v :@ _) = do
    (o, LocalVar{..}) <- getLocalVar True v
    (if allocatedOnHeap then mov else lea) (o `rel` rbp) rbx
    push rbx
    return varType
compileLeftValue (Dot (e :@ le) (m :@ lm) :@ _) = do
    t <- compileLeftValue (e :@ le)
    pop rbx
    s :@ _ <- case t of
        Type t -> return t
        Pointer (Type t) -> do
            mov (0 `rel` rbx) rbx
            return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (o, t') <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return =<< getMember s m
    add (imm o) rbx
    push rbx
    return t'
compileLeftValue (Unary ("*" :@ _) (e :@ l) :@ _) = do
    t <- compileLeftValue (e :@ l)
    case t of
        Pointer t' -> do
            pop rbx
            push (0 `rel` rbx)
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
compileExpression (Variable ("_" :@ l) :@ _) = throwAt l "invalid use of _ in an expression"
compileExpression (Variable v :@ _) = do
    (o, LocalVar{..}) <- getLocalVar True v
    s <- sizeof varType
    sub (imm s) rsp
    if allocatedOnHeap then do
        mov (o `rel` rbp) rbx
        move varType (0 `Rel` rbx) (0 `Rel` rsp)
    else
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
    sub (imm (P.size returnPack)) rsp
    mapM_ allocateVar (P.objects parametersPack)
    compileExpressionsAs ts es
    p <- packTypesUpwards 0 ts
    forM_ (zip (P.downwardsAssocs parametersPack) (P.downwardsAssocs p)) $
        \((po, LocalVar{..}), (eo, t)) -> do
            let po' = po - 16 + P.size p
            if allocatedOnHeap then do
                mov (po' `rel` rsp) rbx
                move t (eo `Rel` rsp) (0 `Rel` rbx)
            else do
                move t (eo `Rel` rsp) (po' `Rel` rsp)
    add (imm (P.size p)) rsp
    call (function f)
    return returns
compileExpression (Print es :@ l) = do
    fmtImported <- gets fmtImported
    unless fmtImported $ throwAt l "fmt used but not imported"
    setFmtUsed
    ts <- compileExpressionsWith compileSimpleExpression es
    p <- packTypesUpwards 0 ts
    printExpressions False (0 `Rel` rsp) (P.downwardsAssocs p)
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
    s <- sizeof t1
    comp t1 (s `Rel` rsp) (0 `Rel` rsp)
    label "0"
    (if op == "==" then sete else setne) al
    movzbq al rax
    add (imm (2 * s)) rsp
    push rax
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

-- Compile une expression avec le type donné.
compileExpressionAs :: Type -> Expression -> FunctionCompiler ()
compileExpressionAs t e@(_ :@ l) = do
    t' <- compileSimpleExpression e
    unless (t' `matches` t) $ throwAt l $ "this expression has type " ++ show t' ++ " but was expected of type " ++ show t

-- Compile une ou plusieurs expressions.
compileExpressionsWith :: (Expression -> FunctionCompiler Type) -> Expressions -> FunctionCompiler [Type]
compileExpressionsWith _ ([e@(Call _ _ :@ _)] :@ _) = compileExpression e
compileExpressionsWith f (es :@ l) = mapM f es

-- Compile une ou plusieurs expressions avec les types donnés.
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
        functions <- gets functions
        mapM_ compileFunction functions
        data'
        compileStringLiterals
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
