{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module CodeGen where

import Control.Monad.Writer
import Data.List

indent = replicate 4 ' '

statement s = tell (s ++ "\n")

directive d = statement ('.':d)

text = directive "text"
data' = directive "data"
global s = directive ("global " ++ s)

label l = statement (l ++ ":")

instruction m o = statement (indent ++ m ++ if null o then "" else ' ':intercalate ", " o)
instruction0 m = instruction m []
instruction1 m a = instruction m [a]
instruction2 m a b = instruction m [a, b]

ret = instruction0 "ret"
[call, leave, push, pop] = instruction1 <$>
    ["call", "leave", "push", "pop"]
[mov, xor] = instruction2 <$>
    ["mov", "xor"]

zero r = xor r r

immediate i = '$':show i

register = ('%':)

[rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi] = register <$>
    ["rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi"]
