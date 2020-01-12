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
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe

import AST hiding (structures, functions, Function(..))
import qualified AST
import Pack (Pack)
import qualified Pack as P
import X86_64

-- This instance allows to access multiple layers of `StateT` without using `lift`,
-- as long as they have different state types.
-- I need it in order to access global state in the same way within `Compiler`
-- and `FunctionCompiler`.
instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (StateT s' m) where
    get = lift get
    put = lift . put

-- Type errors.

type TypeError = Located String

throw e = throwError (e :@ nowhere)
throwAt l e = throwError (e :@ l)
plural 1 thing = thing
plural n thing = thing ++ "s"
0 +++ thing = "no " ++ thing
n +++ thing = show n ++ " " ++ plural n thing

-- Global state.

data GlobalState = GlobalState { structures         :: Map String (Either Fields Structure)
                               , functions          :: Map String Function
                               , fmtImported        :: Bool
                               , fmtUsed            :: Bool
                               , labelCounter       :: Integer
                               , stringLabelCounter :: Integer
                               , strings            :: Map String Operand
                               }

type Structure = Pack Type

data Function = Function { functionName   :: Identifier
                         , parameters     :: Fields
                         , returns        :: [Type]
                         , body           :: [Statement]
                         , parametersPack :: Scope
                         , returnPack     :: Pack Type
                         }

-- Function-local state.

data FunctionState = FunctionState { currentFunction :: Function
                                   , scopes          :: [Scope]
                                   , returned        :: Bool
                                   , returnLabel     :: Operand
                                   }

type Scope = Pack LocalVariable

data LocalVariable = LocalVariable { varType         :: Type
                                   , used            :: Bool
                                   , definedAt       :: Location
                                   , allocatedOnHeap :: Bool
                                   }

type Compiler = StateT GlobalState (WriterT String (Except TypeError))

type FunctionCompiler = StateT FunctionState Compiler

-- Utilities.

hide = censor (const mempty) . listen

getStructure s = do
    structures <- gets structures
    case structures M.! s of
        Right s' -> return s'
getMember s m = (P.!? m) <$> getStructure s

getFunction f = M.lookup f <$> gets functions

getFreshLabel = do
    c <- gets labelCounter
    modify' $ \gs -> gs { labelCounter = succ c }
    return $ Label $ ".L" ++ show c

getStringLiteral "" = return 0
getStringLiteral s = do
    strings <- gets strings
    case strings M.!? s of
        Just l -> return l
        Nothing -> do
            c <- gets stringLabelCounter
            let l = Label $ ".LS" ++ show c
            modify' $ \gs -> gs { strings = M.insert s l strings
                                , stringLabelCounter = succ c }
            return l

getBottom = gets (P.bottom . head . scopes)

getLocalVariable :: Bool -> Identifier -> FunctionCompiler (Integer, LocalVariable)
getLocalVariable used (v :@ l) = do
    scopes <- gets scopes
    let (m, scopes') = mapAccumL f Nothing scopes
    modify' $ \fs -> fs { scopes = scopes' }
    maybe (throwAt l $ "unbound variable " ++ v) return m
    where
        f Nothing s | Just (o, lv) <- s P.!? v =
                        (Just (o, lv), P.update v lv { used } s)
                    | otherwise = (Nothing, s)
        f m s = (m, s)

setReturned returned = modify' $ \fs -> fs { returned }

-- Returns the size of a type, after checking its validity.
sizeOf (Type ("int" :@ _)) = return 8
sizeOf (Type ("bool" :@ _)) = return 8
sizeOf (Type ("string" :@ _)) = return 8
sizeOf (Type (s :@ l)) = do
    structures <- gets structures
    case structures M.!? s of
        Just ~(Right p) -> return (P.size p)
        _               -> throwAt l $ "invalid type " ++ s
sizeOf (Pointer t) = 8 <$ sizeOf t
sizeOf NilType = return 8

-- Builds a `Pack` from a list of types.
packTypesUpwards offset ts = foldrM f (P.emptyAt offset) ts
    where f t p = do s <- sizeOf t; return (P.pushUp s t p)
packTypesDownwards offset ts = foldlM f (P.emptyAt offset) ts
    where f p t = do s <- sizeOf t; return (P.pushDown s t p)

-- Compilation starts here.

compileFile :: File -> Either TypeError String
compileFile (File importFmt structuresList functionsList) =
    runExcept $ execWriterT $ flip evalStateT initialGlobalState $ do
        addStructures structuresList
        mapM_ addFunction functionsList
        text
        compileMain
        functions <- gets functions
        mapM_ compileFunction functions
        data'
        compileStringLiterals
        unless ("main" `M.member` functions) $ throw "no main function"
        fmtUsed <- gets fmtUsed
        case importFmt of
            Just (_ :@ l) | not fmtUsed -> throwAt l "fmt imported but not used"
            _                           -> return ()
    where
        initialGlobalState = GlobalState { structures         = M.empty
                                         , functions          = M.empty
                                         , fmtImported        = isJust importFmt
                                         , fmtUsed            = False
                                         , labelCounter       = 0
                                         , stringLabelCounter = 0
                                         , strings            = M.empty
                                         }

addStructures :: [(Identifier, Fields)] -> Compiler ()
addStructures structuresList = do
    forM_ structuresList $ \(n :@ l, s) -> do
        structures <- gets structures
        when (n `M.member` structures) $ throwAt l $ "duplicate structure " ++ n
        modify' $ \gs -> gs { structures = M.insert n (Left s) structures }
    forM_ structuresList $ \(n :@ _, _) -> addStructure n

addStructure :: String -> Compiler ()
addStructure = go [] where
    go seen n = do
        structs <- gets structures
        case structs M.! n of
            Left fields -> do
                let addField s (f :@ l, t) = do
                        structs <- gets structures
                        case t of
                            Type (n' :@ _) | n' `M.member` structs -> do
                                let seen' = n:seen
                                    cycle = dropWhileEnd (/= n') seen'
                                unless (null cycle) $ throwAt l $ "recursive " ++ plural (length cycle) "structure" ++ " " ++ intercalate ", " cycle
                                go seen' n'
                            _ -> return ()
                        size <- sizeOf t
                        maybe (throwAt l $ "duplicate field " ++ f ++ " in structure " ++ n) return $ P.pushUpWithName f size t s
                structure <- foldlM addField P.empty fields
                modify' $ \gs -> gs { structures = M.insert n (Right structure) (structures gs) }
            _ -> return ()

addFunction :: (Identifier, AST.Function) -> Compiler ()
addFunction (functionName@(n :@ l), AST.Function parameters returns body) = do
    functions <- gets functions
    when (n `M.member` functions) $ throwAt l $ "duplicate function " ++ n
    when (n == "main") $ do
        unless (null parameters) $ throwAt l "main should not take any parameters"
        unless (null returns)    $ throwAt l "main should not return anything"
    let addParameter (v :@ l, t) sc = do
            s <- sizeOf t
            let allocatedOnHeap = shouldAllocateOnHeap v body
                lv = LocalVariable { varType   = t
                                   , used      = True
                                   , definedAt = nowhere
                                   , allocatedOnHeap
                                   }
                s' | allocatedOnHeap = 8
                   | otherwise       = s
            maybe (throwAt l $ "duplicate parameter " ++ v ++ " in function " ++ n) return $
                P.pushUpWithName v s' lv sc
    parametersPack <- foldrM addParameter (P.emptyAt 16) parameters
    returnPack <- packTypesUpwards (P.top parametersPack) returns
    modify' $ \gs -> gs { functions = M.insert n Function{..} functions }

functionLabel :: String -> Operand
functionLabel f = Label $ "pgo_func_" ++ f

compileMain :: Compiler ()
compileMain = do
    global "main"
    label "main"
    call (functionLabel "main")
    zero rax
    ret

compileFunction :: Function -> Compiler ()
compileFunction f@Function{ functionName = n :@ l, .. } = do
    label (functionLabel n)
    push rbp
    mov rsp rbp
    returnLabel <- getFreshLabel
    let initialFunctionState =
            FunctionState { currentFunction = f
                          , scopes          = [parametersPack { P.bottom = 0 }]
                          , returned        = False
                          , returnLabel
                          }
    FunctionState{..} <- execStateT (compileStatements body) initialFunctionState
    unless (null returns || returned) $
        throwAt l $ "missing return statement in function " ++ n
    label returnLabel
    leave
    case P.size parametersPack of
        0 -> ret
        s -> ret1 (Imm s)

-- Data manipulation utilities.

-- Recursively run `f` on all fields of a structure.
recursively :: (Operand -> Operand -> FunctionCompiler ())
            -> Type
            -> Operand -> Operand -> FunctionCompiler ()
recursively f (Type ("int" :@ _)) = f
recursively f (Type ("bool" :@ _)) = f
recursively f (Type ("string" :@ _)) = f
recursively f (Type (s :@ _)) = \a b -> do
    struct <- getStructure s
    forM_ (P.upwardsObjects struct) $ \(o, t) ->
        recursively f t (a `shift` o) (b `shift` o)
recursively f (Pointer _) = f
recursively f NilType = f

-- Check two objects for equality, with short-circuit.
comp :: Type -> Operand -> Operand -> FunctionCompiler ()
comp = recursively compQuad where
    compQuad a b = do
        mov a rax
        cmp b rax
        jne "0f"

-- Move an object from one address to the other.
move :: Type -> Operand -> Operand -> FunctionCompiler ()
move = recursively moveQuad where
    moveQuad s@Rel{} t@Rel{} = mov s rax >> mov rax t
    moveQuad s t = mov s t

-- Set an object to zero.
clear :: Type -> Operand -> FunctionCompiler ()
clear t = move t 0

-- Printing utilities.

printCharacter c = do
    mov (Imm (fromIntegral $ ord c)) rdi
    call "putchar"

printNil = do
    fmt <- getStringLiteral "<nil>"
    mov fmt rdi
    zero rax
    call "printf"

printExpressions :: Bool -> Operand -> [(Integer, Type)] -> FunctionCompiler ()
printExpressions alwaysSpace s es = do
    flip evalStateT False $ forM_ es $ \(o, t) ->
        printExpression alwaysSpace (s `shift` o) t

printExpression :: Bool -> Operand -> Type -> StateT Bool FunctionCompiler ()
printExpression alwaysSpace o t = do
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
            mov o rsi
            zero rax
            call "printf"
        Type ("bool" :@ _) -> do
            false <- getStringLiteral "false"
            true <- getStringLiteral "true"
            mov false rax
            mov true rbx
            cmp 0 o
            cmove rax rdi
            cmovne rbx rdi
            zero rax
            call "printf"
        Type ("string" :@ _) -> do
            mov o rax
            cmp 0 rax
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
            lift $ printExpressions True o (P.upwardsObjects struct)
            printCharacter '}'
        NilType -> printNil
        Pointer _ -> do
            mov o rax
            cmp 0 rax
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

-- If `v` appears as an operand to the address-of (`&`) operator in `b`, we should
-- allocate it on the heap.
shouldAllocateOnHeap :: String -> [Statement] -> Bool
shouldAllocateOnHeap v b = any checkStatement b where
    checkStatement (Expression e) = checkExpression e
    checkStatement (Var _ _ (es :@ _)) = any checkExpression es
    checkStatement (Assign _ (es :@ _)) = any checkExpression es
    checkStatement (Return (es :@ _)) = any checkExpression es
    checkStatement (Block b) = any checkStatement b
    checkStatement (If cond yes no) = checkExpression cond || any checkStatement yes || any checkStatement no
    checkStatement (For cond body) = checkExpression cond || any checkStatement body
    checkStatement _ = False
    checkExpression (Dot e _ :@ _) = checkExpression e
    checkExpression (Call _ (es :@ _) :@ _) = any checkExpression es
    checkExpression (Print (es :@ _) :@ _) = any checkExpression es
    checkExpression (Unary ("&" :@ _) e :@ _) = checkLeftValue e
    checkExpression (Unary _ e :@ _) = checkExpression e
    checkExpression (Binary _ e1 e2 :@ _) = checkExpression e1 || checkExpression e2
    checkExpression _ = False
    checkLeftValue (Variable (v' :@ _) :@ _) = v == v'
    checkLeftValue (Dot e _ :@ _) = checkLeftValue e
    checkLeftValue _ = False

allocateOnHeap :: Type -> FunctionCompiler ()
allocateOnHeap t = do
    s <- sizeOf t
    mov (Imm s) rdi
    call "sbrk"
    push rax
    mov rax rbx
    clear t (0 `Rel` rbx)

-- Allocates a local variable and returns the size it occupies on the stack.
allocateLocalVariable :: LocalVariable -> FunctionCompiler Integer
allocateLocalVariable LocalVariable{ allocatedOnHeap = True, .. } = 8 <$ allocateOnHeap varType
allocateLocalVariable LocalVariable{..} = do
    s <- sizeOf varType
    sub (Imm s) rsp
    clear varType (0 `Rel` rsp)
    return s

-- Compiles a block in a new scope.
compileBlock :: [Statement] -> FunctionCompiler ()
compileBlock b = do
    bottom <- getBottom
    modify' $ \fs -> fs { scopes = P.emptyAt bottom:scopes fs }
    compileStatements b
    modify' $ \fs -> fs { scopes = tail (scopes fs) }
    lea (bottom `Rel` rbp) rsp

-- Compiles all statements in a block, then checks for unused variables.
compileStatements :: [Statement] -> FunctionCompiler ()
compileStatements (s:b) = compileStatement s b >> compileStatements b
compileStatements [] = do
    vars <- gets (P.namedObjects . head . scopes)
    sequence_ [ throwAt l $ "unused variable " ++ v
              | (v, LocalVariable { used = False, definedAt = l }) <- vars ]

-- Compiles a statement. The second argument is the rest of the block; we need it
-- for escape analysis.
compileStatement :: Statement -> [Statement] -> FunctionCompiler ()
compileStatement (Block b) _ = compileBlock b
compileStatement (Expression e) _ = do
    compileExpression e
    bottom <- getBottom
    lea (bottom `Rel` rbp) rsp
compileStatement (Increment e) _ = do
    compileAddressAs "int" e
    pop rbx
    mov (0 `Rel` rbx) rax
    inc rax
    mov rax (0 `Rel` rbx)
compileStatement (Decrement e) _ = do
    compileAddressAs "int" e
    pop rbx
    mov (0 `Rel` rbx) rax
    dec rax
    mov rax (0 `Rel` rbx)
compileStatement (Var vs mt es@(_ :@ l)) b = do
    -- Compile the expressions first so that we know what types the variables
    -- should have, but hide the output because we need to allocate the variables
    -- first.
    (ts, compiledExpressions) <- hide $ case mt of
        Just t -> (t <$ vs) <$ case es of
            [] :@ _ -> return ()
            _ -> compileExpressionsAs (t <$ vs) es
        Nothing -> compileExpressionsWith compileConcreteExpression es
    unless (length ts == length vs) $ throwAt l $
        "expected " ++ length vs +++ "value" ++ ", got " ++ show (length ts)
    -- Allocate the variables.
    forM_ (zip vs ts) $ \(v :@ l, t) -> case v of
        "_" -> return ()
        _ -> do
            let allocatedOnHeap = shouldAllocateOnHeap v b
                lv = LocalVariable { varType   = t
                                   , used      = False
                                   , definedAt = l
                                   , allocatedOnHeap
                                   }
            s <- allocateLocalVariable lv
            ~(sc:scs) <- gets scopes
            sc' <- maybe (throwAt l $ "redefined variable " ++ v) return $
                P.pushDownWithName v s lv sc
            modify' $ \fs -> fs { scopes = sc':scs }
    -- Output the compiled expressions and assign them to the variables.
    unless (null compiledExpressions) $ do
        tell compiledExpressions
        bottom <- getBottom
        p <- packTypesDownwards bottom ts
        forM_ (zip vs (P.downwardsObjects p)) $ \(v, (eo, t)) -> case v of
            "_" :@ _ -> return ()
            _ -> do
                (vo, LocalVariable{..}) <- getLocalVariable False v
                if allocatedOnHeap then do
                    mov (vo `Rel` rbp) rbx
                    move t (eo `Rel` rbp) (0 `Rel` rbx)
                else do
                    move t (eo `Rel` rbp) (vo `Rel` rbp)
        lea (bottom `Rel` rbp) rsp
compileStatement (Assign vs es) _ = do
    bottom <- getBottom
    ts <- compileExpressionsWith compileSimpleExpression es
    pe <- packTypesDownwards bottom ts
    forM_ (zip vs (P.downwardsObjects pe)) $ \(v, (o, t)) -> case v of
        Variable ("_" :@ _) :@ _ -> return ()
        _ -> do
            compileAddressAs t v
            pop rbx
            move t (o `Rel` rbp) (0 `Rel` rbx)
    lea (bottom `Rel` rbp) rsp
compileStatement (Return es) _ = do
    Function{..} <- gets currentFunction
    bottom <- getBottom
    compileExpressionsAs returns es
    pe <- packTypesDownwards bottom returns
    let f (ro, _) (eo, t) = do
            move t (eo `Rel` rbp) (ro `Rel` rbp)
    zipWithM f (P.downwardsObjects returnPack) (P.downwardsObjects pe)
    returnLabel <- gets returnLabel
    jmp returnLabel
    setReturned True
compileStatement (If cond yes no) _ = do
    compileExpressionAs "bool" cond
    pop rax
    cmp 0 rax
    labelFalse <- getFreshLabel
    labelEnd <- getFreshLabel
    je labelFalse
    returned' <- gets returned
    compileBlock yes
    yesReturned <- gets returned
    setReturned returned'
    jmp labelEnd
    label labelFalse
    compileBlock no
    noReturned <- gets returned
    setReturned (yesReturned && noReturned)
    label labelEnd
compileStatement (For cond b) _ = do
    labelBody <- getFreshLabel
    labelCond <- getFreshLabel
    jmp labelCond
    label labelBody
    returned' <- gets returned
    compileBlock b
    setReturned returned'
    label labelCond
    compileExpressionAs "bool" cond
    pop rax
    cmp 0 rax
    jne labelBody

-- Compile a left value and pushes its address to the stack.
compileAddress :: Expression -> FunctionCompiler Type
compileAddress (Variable ("_" :@ l) :@ _) = throwAt l "invalid use of _ as a left value"
compileAddress (Variable v :@ _) = do
    (o, LocalVariable{..}) <- getLocalVariable True v
    (if allocatedOnHeap then mov else lea) (o `Rel` rbp) rbx
    push rbx
    return varType
compileAddress (Dot (e :@ le) (m :@ lm) :@ _) = do
    t <- compileAddress (e :@ le)
    pop rbx
    s :@ _ <- case t of
        Type t -> return t
        Pointer (Type t) -> do
            mov (0 `Rel` rbx) rbx
            return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (o, tm) <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return =<< getMember s m
    add (Imm o) rbx
    push rbx
    return tm
compileAddress (Unary ("*" :@ _) (e :@ l) :@ _) = do
    t <- compileSimpleExpression (e :@ l)
    case t of
        Pointer t' -> return t'
        NilType -> throwAt l "cannot dereference nil"
        _ -> throwAt l $ "cannot dereference a value of type " ++ show t
compileAddress (e :@ l) = throwAt l "this expression is not a left value"

-- Compiles a left value of the given type.
compileAddressAs :: Type -> Expression -> FunctionCompiler ()
compileAddressAs t e@(_ :@ l) = do
    t' <- compileAddress e
    unless (t `matches` t') $ throwAt l $
        "this value has type " ++ show t' ++ " but was expected of type " ++ show t

-- Compiles an expression and pushes its value to the stack.
compileExpression :: Expression -> FunctionCompiler [Type]
compileExpression (Int i :@ _) = do
    mov (Imm i) rax
    push rax
    return ["int"]
compileExpression (String s :@ _) = do
    l <- getStringLiteral s
    push l
    return ["string"]
compileExpression (Bool b :@ _) = do
    push (Imm (if b then 1 else 0))
    return ["bool"]
compileExpression (Nil :@ _) = do
    push 0
    return [NilType]
compileExpression (Variable ("_" :@ l) :@ _) = throwAt l "invalid use of _ in an expression"
compileExpression (Variable v :@ _) = do
    (o, LocalVariable{..}) <- getLocalVariable True v
    s <- sizeOf varType
    sub (Imm s) rsp
    if allocatedOnHeap then do
        mov (o `Rel` rbp) rbx
        move varType (0 `Rel` rbx) (0 `Rel` rsp)
    else
        move varType (o `Rel` rbp) (0 `Rel` rsp)
    return [varType]
compileExpression (Dot e@(_ :@ le) (m :@ lm) :@ _) = do
    t <- compileSimpleExpression e
    st <- sizeOf t
    s :@ _ <- case t of
        Type t -> do
            mov rsp rbx
            add (Imm st) rsp
            return t
        Pointer (Type t) -> do
            pop rbx
            return t
        _ -> throwAt le $ "type " ++ show t ++ " cannot be accessed"
    (o, tm) <- maybe (throwAt lm $ "no member " ++ m ++ " in type " ++ s) return =<< getMember s m
    stm <- sizeOf tm
    sub (Imm stm) rsp
    move tm (o `Rel` rbx) (0 `Rel` rsp)
    return [tm]
compileExpression (Call ("new" :@ _) ([Variable n :@ _] :@ _) :@ _) = do
    let t = Type n
    allocateOnHeap t
    return [Pointer t]
compileExpression (Call ("new" :@ _) (_ :@ l) :@ _) = do
    throwAt l "new() expects a single type name"
compileExpression (Call (f :@ l) es :@ _) = do
    Function{..} <- maybe (throwAt l $ "undefined function " ++ f) return =<< getFunction f
    let ts = map snd parameters
    sub (Imm (P.size returnPack)) rsp
    mapM_ allocateLocalVariable (P.objects parametersPack)
    compileExpressionsAs ts es
    p <- packTypesUpwards 0 ts
    forM_ (zip (P.downwardsObjects parametersPack) (P.downwardsObjects p)) $
        \((po, LocalVariable{..}), (eo, t)) -> do
            let po' = po - 16 + P.size p
            if allocatedOnHeap then do
                mov (po' `Rel` rsp) rbx
                move t (eo `Rel` rsp) (0 `Rel` rbx)
            else do
                move t (eo `Rel` rsp) (po' `Rel` rsp)
    add (Imm (P.size p)) rsp
    call (functionLabel f)
    return returns
compileExpression (Print es :@ l) = do
    fmtImported <- gets fmtImported
    unless fmtImported $ throwAt l "fmt used but not imported"
    modify' $ \gs -> gs { fmtUsed = True }
    ts <- compileExpressionsWith compileSimpleExpression es
    p <- packTypesUpwards 0 ts
    printExpressions False (0 `Rel` rsp) (P.downwardsObjects p)
    add (Imm (P.size p)) rsp
    return []
compileExpression (Unary ("!" :@ _) e :@ _) = do
    compileExpressionAs "bool" e
    pop rax
    xor 1 rax
    push rax
    return ["bool"]
compileExpression (Unary ("-" :@ _) e :@ _) = do
    compileExpressionAs "int" e
    pop rax
    neg rax
    push rax
    return ["int"]
compileExpression e@(Unary ("*" :@ _) _ :@ _) = do
    t <- compileAddress e
    s <- sizeOf t
    pop rbx
    sub (Imm s) rsp
    move t (0 `Rel` rbx) (0 `Rel` rsp)
    return [t]
compileExpression (Unary ("&" :@ _) e :@ _) = do
    t <- compileAddress e
    return [Pointer t]
compileExpression (Unary (op :@ l) _ :@ _) = throwAt l $ "unknown unary operator " ++ op
compileExpression (Binary (op :@ _) (Nil :@ _) (Nil :@ _) :@ l) | op `elem` ["==", "!="] = do
    throwAt l "cannot compare nil with nil"
compileExpression (Binary (op :@ _) e1 e2 :@ l) | op `elem` ["==", "!="] = do
    t1 <- compileSimpleExpression e1
    t2 <- compileSimpleExpression e2
    unless (t1 `matches` t2) $ throwAt l $ "cannot match types " ++ show t1 ++ " and " ++ show t2
    s <- sizeOf t1
    comp t1 (s `Rel` rsp) (0 `Rel` rsp)
    label "0"
    (if op == "==" then sete else setne) al
    movzbq al rax
    add (Imm (2 * s)) rsp
    push rax
    return ["bool"]
compileExpression (Binary (op :@ _) e1 e2 :@ _) | op `elem` ["<", "<=", ">", ">="] = do
    compileExpressionAs "int" e1
    compileExpressionAs "int" e2
    pop rbx
    pop rax
    cmp rbx rax
    (case op of
        "<"  -> setl
        "<=" -> setle
        ">"  -> setg
        ">=" -> setge) al
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
    (case op of
        "&&" -> and'
        "||" -> or') rbx rax
    push rax
    return ["bool"]
compileExpression (Binary (op :@ l) _ _ :@ _) = throwAt l $ "unknown binary operator " ++ op

-- Compiles a simple expression, i.e. an expression with exactly one type.
compileSimpleExpression :: Expression -> FunctionCompiler Type
compileSimpleExpression e@(_ :@ l) = do
    ts <- compileExpression e
    case ts of
        [t] -> return t
        _ -> throwAt l $ "this expression has " ++ length ts +++ "value" ++ " but a single value was expected"

-- Compiles a concrete expression, i.e. a simple expression other than nil.
compileConcreteExpression :: Expression -> FunctionCompiler Type
compileConcreteExpression (Nil :@ l) = throwAt l "nil has no type"
compileConcreteExpression e = compileSimpleExpression e

-- Compiles an expression of the given type.
compileExpressionAs :: Type -> Expression -> FunctionCompiler ()
compileExpressionAs t e@(_ :@ l) = do
    t' <- compileSimpleExpression e
    unless (t' `matches` t) $ throwAt l $ "this expression has type " ++ show t' ++ " but was expected of type " ++ show t

-- Compiles zero or more expressions.
compileExpressionsWith :: (Expression -> FunctionCompiler Type) -> Expressions -> FunctionCompiler [Type]
compileExpressionsWith _ ([e@(Call _ _ :@ _)] :@ _) = compileExpression e
compileExpressionsWith f (es :@ l) = mapM f es

-- Compiles zero or more expressions with the given types.
compileExpressionsAs :: [Type] -> Expressions -> FunctionCompiler ()
compileExpressionsAs ts ([e@(Call _ _ :@ l)] :@ _) | length ts > 1 = do
    ts' <- compileExpression e
    unless (length ts' == length ts) $ throwAt l $ "this function returns " ++ length ts' +++ "value" ++ " but " ++ length ts +++ "value" ++ " were expected"
    unless (and $ zipWith matches ts ts') $ throwAt l $ "couldn't match type " ++ show ts' ++ " with expected type " ++ show ts
compileExpressionsAs ts (es :@ l) = do
    unless (length es == length ts) $ throwAt l $ "expected " ++ length ts +++ "expression" ++ ", got " ++ show (length es)
    zipWithM_ compileExpressionAs ts es

-- Checks for compatibility between types.
matches :: Type -> Type -> Bool
NilType   `matches` NilType   = False
NilType   `matches` Pointer _ = True
Pointer _ `matches` NilType   = True
a         `matches` b         = a == b

compileStringLiterals :: Compiler ()
compileStringLiterals = do
    strings <- gets strings
    forM_ (M.assocs strings) $ \(s, l) -> do
        label l
        string s
