{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module X86_64 where

import Control.Monad.Writer
import Data.List

indent = replicate 4 ' '

emit s = tell (s ++ "\n")
emitIndented s = tell (indent ++ s ++ "\n")

directive d = emit ('.':d)

text = directive "text"
data_ = directive "data"
global s = directive ("global " ++ s)
string s = directive ("string " ++ makeString s)
    where
        makeString s = "\"" ++ s ++ "\"" -- TODO

label l = emit (l ++ ":")

ins m [] = emitIndented m
ins m o = emitIndented (m ++ " " ++ intercalate ", " o)
ins0 m = ins m []
ins1 m a = ins m [a]
ins2 m a b = ins m [a, b]

[leave, ret] = ins0 <$>
    ["leave", "ret"]
[call, ret1, push, pop] = ins1 <$>
    ["call", "ret", "push", "pop"]
[mov, lea, add, sub, xor] = ins2 <$>
    ["mov", "lea", "add", "sub", "xor"]

zero r = xor r r

imm i = '$':show i
immLabel l = '$':l

rel disp base = show disp ++ "(" ++ base ++ ")"

register = ('%':)

[rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi] = register <$>
    ["rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi"]
