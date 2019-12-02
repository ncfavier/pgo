module AST where

import Data.List

data Type = Type String
          | Pointer Type
          deriving Eq

instance Show Type where
    show (Type t) = t
    show (Pointer t) = '*':show t
    showList ts = showChar '(' . showString (intercalate ", " (map show ts)) . showChar ')'

type Fields = [(String, Type)]

data File = File { importFmt  :: Bool
                 , structures :: [(String, Fields)]
                 , functions  :: [(String, Function)]
                 }

data Function = Function { parameters :: Fields
                         , returns    :: [Type]
                         , body       :: [Statement]
                         }

data Statement = Expression Expression
               | Increment Expression
               | Decrement Expression
               | Var [String] (Maybe Type) [Expression]
               | Assign [Expression] [Expression]
               | Return [Expression]
               | Block [Statement]
               | If Expression [Statement] [Statement]
               | For Expression [Statement]

data Expression = Int Integer
                | String String
                | Boolean Bool
                | Nil
                | Variable String
                | Dot Expression String
                | Call String [Expression]
                | Print [Expression]
                | Unary String Expression
                | Binary String Expression Expression
