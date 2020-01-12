module AST where

import Data.List
import Data.String
import Text.Parsec
import Text.Parsec.Pos

type Location = Maybe (SourcePos, SourcePos)

infix 5 :@
data Located a = (:@) { forgetLocation :: a, getLocation :: Location }

nowhere :: Location
nowhere = Nothing

(++@) :: Located a -> Located b -> Location
(_ :@ Just (start, _)) ++@ (_ :@ Just (_, end)) = Just (start, end)
_ ++@ _ = Nothing

data Type = Type Identifier
          | Pointer Type
          | Any

type Identifier = Located String
type Operator = Located String

type Fields = [(Identifier, Type)]

data File = File { importFmt  :: Maybe (Located ())
                 , structures :: [(Identifier, Fields)]
                 , functions  :: [(Identifier, Function)]
                 }

data Function = Function { parameters :: Fields
                         , returns    :: [Type]
                         , body       :: [Statement]
                         }

data Statement = Expression Expression
               | Increment Expression
               | Decrement Expression
               | Var [Identifier] (Maybe Type) Expressions
               | Assign [Expression] Expressions
               | Return Expressions
               | Block [Statement]
               | If Expression [Statement] [Statement]
               | For Expression [Statement]

data Expression' = Int Integer
                 | String String
                 | Bool Bool
                 | Nil
                 | Variable Identifier
                 | Dot Expression Identifier
                 | Call Identifier Expressions
                 | Print Expressions
                 | Unary Operator Expression
                 | Binary Operator Expression Expression

type Expression = Located Expression'
type Expressions = Located [Expression]

instance Eq a => Eq (Located a) where
    a :@ _ == b :@ _ = a == b

instance Eq Type where
    Type a    == Type b    = a == b
    Pointer a == Pointer b = a == b
    Any       == _         = True
    _         == Any       = True
    _         == _         = False

instance Show Type where
    show (Type (t :@ _)) = t
    show (Pointer Any) = "nil"
    show (Pointer t) = '*':show t
    showList ts = showParen (length (take 2 ts) /= 1) $
        showString (intercalate ", " (map show ts))

instance IsString Type where
    fromString t = Type (t :@ nowhere)
