{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module X86_64 where

import Control.Monad.Writer
import Data.List

indent = replicate 4 ' '

emit s = tell (s ++ "\n")
emitIndented s = tell (indent ++ s ++ "\n")

directive d = emit ('.':d)
comment c = emit ('#':c)

text = directive "text"
data' = directive "data"
global s = directive ("global " ++ s)
string s = directive ("string " ++ show s)

label l = emit (l ++ ":")

ins m [] = emitIndented m
ins m o = emitIndented (m ++ " " ++ intercalate ", " o)
ins0 m = ins m []
ins1 m a = ins m [a]
ins2 m a b = ins m [a, b]

[leave, ret, cqto] = ins0 <$>
    ["leave", "ret", "cqto"]
[call, ret1, push, pop, jmp, je, jne, sete, setne, setl, setle, setg, setge, inc, dec, neg, idiv] = ins1 <$>
    ["call", "ret", "push", "pop", "jmp", "je", "jne", "sete", "setne", "setl", "setle", "setg", "setge", "inc", "dec", "neg", "idiv"]
[mov, movzbq, cmove, cmovne, lea, add, sub, imul, and', or', xor, cmp] = ins2 <$>
    ["movq", "movzbq", "cmove", "cmovne", "lea", "add", "sub", "imul", "and", "or", "xor", "cmp"]

zero r = xor r r

imm i = '$':show i
immLabel l = '$':l
immBool b = '$':if b then "1" else "0"

disp `rel` base = show disp ++ "(" ++ base ++ ")"

register = ('%':)

[al, rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi] = register <$>
    ["al", "rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi"]

infix 3 `Rel`
data Relative = Integer `Rel` String
