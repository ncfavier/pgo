module AST where

import Data.List
import Data.String

data Type = Type String
          | Pointer Type
          | NilType
          deriving Eq

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

instance Show Type where
    show (Type t) = t
    show (Pointer t) = '*':show t
    show NilType = "nil"
    showList ts = showParen (length (take 2 ts) /= 1) $
        showString (intercalate ", " (map show ts))

instance IsString Type where
    fromString = Type
