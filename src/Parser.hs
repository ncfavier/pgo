{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Control.Monad
import Data.Int
import Data.Char
import Data.Either
import Text.Parsec hiding (digit, hexDigit)

type Identifier = String
type Operator = String

data Type = Tvar Identifier
          | Pointer Type

data File = File { importFmt :: Bool, structures :: [(Identifier, Signature)], functions :: [(Identifier, Function)] }

type Signature = [(Identifier, Type)]

data Function = Function Signature [Type] [Instruction]

data Instruction = Expression Expression
                 | Increment Expression | Decrement Expression
                 | Var [Identifier] (Maybe Type) [Expression]
                 | [Expression] :=: [Expression]
                 | Return [Expression]
                 | Block [Instruction]
                 | If Expression [Instruction] [Instruction]
                 | For Expression [Instruction]

data Expression = Int Int64
                | String String
                | Boolean Bool
                | Nil
                | Variable Identifier
                | Expression :.: Identifier
                | Call Identifier [Expression]
                | Print [Expression]
                | Unary Operator Expression
                | Binary Operator Expression Expression

data Lexeme = IntLit Int64
            | StringLit String
            | Identifier String
            | Keyword String
            | Operator String
            | Punctuation String
            deriving (Show)

autoSemicolon (IntLit _) = True
autoSemicolon (StringLit _) = True
autoSemicolon (Identifier _) = True
autoSemicolon (Keyword "true") = True
autoSemicolon (Keyword "false") = True
autoSemicolon (Keyword "nil") = True
autoSemicolon (Keyword "return") = True
autoSemicolon (Operator "++") = True
autoSemicolon (Operator "--") = True
autoSemicolon (Punctuation ")") = True
autoSemicolon (Punctuation "}") = True
autoSemicolon _ = False

optionBool p = option False (True <$ p)

whitespace = void (oneOf " \t\r\n") <|> lineComment <|> blockComment
lineComment = void $ try (string "//") >> anyChar `manyTill` newline
blockComment = void $ try (string "/*") >> anyChar `manyTill` try (string "*/")

punctuation = (\c -> Punctuation [c]) <$> oneOf ",.;(){}"

alpha = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_' <?> "an alphanumeric character"

reserved = ["else", "false", "for", "func", "if", "import", "nil", "package", "return", "struct", "true", "type", "var"]

identifier = do
    c <- alpha
    cs <- many (alpha <|> digit)
    let s = c:cs
    return $ (if s `elem` reserved then Keyword else Identifier) s

digit = satisfy isDigit <?> "a digit"
hexDigit = (\c -> ord c - ord '0')      <$> satisfy isDigit
       <|> (\c -> ord c - ord 'a' + 10) <$> oneOf "abcdef"
       <|> (\c -> ord c - ord 'A' + 10) <$> oneOf "ABCDEF"

integer = do
    n <- try hexadecimal <|> decimal
    if n > toInteger (maxBound :: Int64) then fail "integer constants must fit in 64 bits" else
        return $ IntLit (fromInteger n)
    where
        decimal = read <$> many1 digit
        hexadecimal = do
            try (string "0x") <|> string "0X"
            foldl (\r d -> toInteger d + 16 * r) 0 <$> many1 hexDigit

stringLit = do
    char '"'
    s <- ((char '\\' >> (choice escapeSequences <|> fail "unknown escape sequence")) <|> anyChar) `manyTill` char '"'
    return (StringLit s)
    where
        escapeSequences = [char '\\', char '"', '\n' <$ char 'n', '\t' <$ char 't']

operator = Operator <$> many1 (oneOf "+-|&=!<>*/%.:")

lexeme = try $ do
    flag <- getState
    line <- sourceLine <$> getPosition
    skipMany whitespace
    line' <- sourceLine <$> getPosition
    let lexAutoSemicolon = Punctuation ";" <$ (guard flag >> (guard (line' > line) <|> eof))
    l <- lexAutoSemicolon <|> integer <|> stringLit <|> identifier <|> punctuation <|> operator
    putState (autoSemicolon l)
    return l

comma = do Punctuation "," <- lexeme; return ()
semicolon = do Punctuation ";" <- lexeme; return ()

inParens = between (do Punctuation "(" <- lexeme; return ()) (do Punctuation ")" <- lexeme; return ())
inBraces = between (do Punctuation "{" <- lexeme; return ()) (do Punctuation "}" <- lexeme; return ())

parseType = (do Operator "*" <- lexeme; Pointer <$> parseType) <|> (do Identifier t <- lexeme; return (Tvar t))

returnType = (:[]) <$> parseType <|> inParens (parseType `sepEndBy1` comma)

varsAndType = do
    vs <- (do Identifier v <- lexeme; return v) `sepBy1` comma
    t <- parseType
    return (vs, t)

makeSignature l = [(v, t) | (vs, t) <- l, v <- vs]

structure = do
    Keyword "type" <- lexeme
    Identifier name <- lexeme
    Keyword "struct" <- lexeme
    l <- inBraces $ varsAndType `sepEndBy` semicolon
    semicolon
    return (name, makeSignature l)

block = inBraces $ instruction `sepEndBy` semicolon

-- instruction = simpleInstruction <|> Block <$> block <|> ifInstruction <|>

function = do
    Keyword "func" <- lexeme
    Identifier name <- lexeme
    params <- inParens $ varsAndType `sepEndBy` comma
    rt <- option [] returnType
    body <- block
    semicolon
    return $ (name, Function (makeSignature params) rt body)

file = do
    Keyword "package" <- lexeme; Identifier "main" <- lexeme; semicolon
    fmt <- optionBool $ do Keyword "import" <- lexeme; StringLit "fmt" <- lexeme; semicolon
    l <- many $ Left <$> structure <|> Right <$> function
    eof
    let (structs, funcs) = partitionEithers l
    return $ File fmt structs funcs

parseGo = runParser file False
