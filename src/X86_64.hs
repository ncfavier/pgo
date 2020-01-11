{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module X86_64 where

import Control.Monad.Writer
import Data.String
import Data.List

infix 3 `Rel`
data Operand = Imm Integer
             | Label String
             | Register String
             | Rel Integer Operand

(d `Rel` b) `shift` o = d + o `Rel` b
o `shift` _ = o

instance Show Operand where
    show (Imm i)      = '$':show i
    show (Label l)    = '$':l
    show (Register r) = '%':r
    show (Rel d b)    = show d ++ "(" ++ show b ++ ")"

instance Num Operand where
    fromInteger = Imm
    (+)         = undefined
    (*)         = undefined
    negate      = undefined
    abs         = undefined
    signum      = undefined

instance IsString Operand where
    fromString = Label

indent = replicate 4 ' '

emit s = tell (s ++ "\n")
emitIndented s = tell (indent ++ s ++ "\n")

directive d = emit ('.':d)
comment c = emit ('#':c)

text = directive "text"
data' = directive "data"
global s = directive ("global " ++ s)
string s = directive ("string " ++ show s)

label (Label l) = emit (l ++ ":")

ins :: MonadWriter String m => String -> [Operand] -> m ()
ins m [] = emitIndented m
ins m o  = emitIndented (m ++ " " ++ intercalate ", " (map show o))
ins0 m = ins m []
ins1 m a = ins m [a]
ins2 m a b = ins m [a, b]
insLabel m (Label l) = emitIndented (m ++ " " ++ l)

[leave, ret, cqto] = ins0 <$>
    ["leave", "ret", "cqto"]
[ret1, push, pop, sete, setne, setl, setle, setg, setge, inc, dec, neg, idiv] = ins1 <$>
    ["ret", "push", "pop", "sete", "setne", "setl", "setle", "setg", "setge", "inc", "dec", "neg", "idiv"]
[mov, movzbq, cmove, cmovne, lea, add, sub, imul, and', or', xor, cmp] = ins2 <$>
    ["movq", "movzbq", "cmove", "cmovne", "lea", "add", "sub", "imul", "and", "or", "xor", "cmp"]
[call, jmp, je, jne] = insLabel <$>
    ["call", "jmp", "je", "jne"]

zero r = xor r r

immBool b = Imm (if b then 1 else 0)

[al, rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi] = Register <$>
    ["al", "rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi"]
