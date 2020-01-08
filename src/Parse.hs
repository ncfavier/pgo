{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Parse (parseFile) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Int
import Data.Char
import Data.String
import Text.Parsec hiding (digit, hexDigit)
import Text.Parsec.Expr
import Text.Parsec.Pos

import AST

data ParserState = ParserState { autoSemicolon :: Bool
                               , lexemeEnd     :: SourcePos
                               }

initialState = ParserState False (initialPos "")

type Parser = Parsec String ParserState

instance (a ~ String) => IsString (Parser a) where
    fromString = keyword

getAutoSemicolon = autoSemicolon <$> getState
setAutoSemicolon b = modifyState $ \s -> s { autoSemicolon = b }

getLexemeEnd = lexemeEnd <$> getState
setLexemeEnd p = modifyState $ \s -> s { lexemeEnd = p }

l = flip label

single x = [x]

optionBool p = option False (True <$ p)

lineComment = try (string "//") >> anyChar `manyTill` (() <$ newline <|> eof)
blockComment = try (string "/*") >> anyChar `manyTill` try (string "*/")
whitestuff = () <$ oneOf " \t\r\n" <|> () <$ lineComment <|> () <$ blockComment <?> "whitespace"
whitespace = skipMany whitestuff

lexeme final p = do
    guard . not =<< getAutoSemicolon
    l <- p
    pos <- getPosition
    setLexemeEnd pos
    let lineBefore = sourceLine pos
    whitespace
    lineAfter <- sourceLine <$> getPosition
    setAutoSemicolon (lineAfter > lineBefore && final)
    return l

located p = do
   start <- getPosition
   r <- p
   end <- getLexemeEnd
   return $ r :@ (start, end)

symbol s = lexeme final $ l (show s) $ string s
    where final = s `elem` [")", "}"]

operator op = located $ lexeme final $ l (show op) $ string op <* notFollowedBy (oneOf "=<>+-*/%&|!")
    where final = op `elem` ["++", "--"]

semicolon = ";" <$ (getAutoSemicolon >>= guard >> setAutoSemicolon False) <|> symbol ";" <?> "a semicolon"
comma = symbol "," <?> "a comma"

inParens = between (symbol "(") (symbol ")")
inBraces = between (symbol "{") (symbol "}")

alpha = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_' <?> "a letter"

digit = satisfy isDigit <?> "a digit"

word = liftA2 (:) alpha (many (alpha <|> digit))

reserved = ["else", "false", "for", "func", "if", "import", "nil", "package", "return", "struct", "true", "type", "var"]

identifier = located $ lexeme True $ l "an identifier" $ do
    w <- word
    guard (w `notElem` reserved)
    return w

keyword :: String -> Parser String
keyword kw = try $ lexeme final $ l (show kw) $ do
    w <- word
    guard (w == kw)
    return w
    where final = kw `elem` ["true", "false", "nil", "return"]

integer = lexeme True $ l "an integer" $ do
    n <- try hexadecimal <|> decimal
    if n > toInteger (maxBound :: Int64) + 1 then fail "integer constants must fit in 64 bits" else
        return n
    where
        decimal = read <$> many1 digit
        hexadecimal = do
            try (string "0x") <|> string "0X"
            foldl (\r d -> toInteger d + 16 * r) 0 <$> many1 hexDigit
        hexDigit = (\c -> ord c - ord '0')      <$> satisfy isDigit
               <|> (\c -> ord c - ord 'a' + 10) <$> oneOf "abcdef"
               <|> (\c -> ord c - ord 'A' + 10) <$> oneOf "ABCDEF"

stringLiteral = lexeme True $ l "a string literal" $ char '"' >> (escapeSequence <|> anyChar) `manyTill` char '"'
    where
        escapeSequence = char '\\' >> (choice [r <$ char c | (c, r) <- sequences] <|> fail "unknown escape sequence")
        sequences = [('\\', '\\'), ('"', '"'), ('n', '\n'), ('t', '\t')]

type' = (symbol "*" >> Pointer <$> type') <|> Type <$> identifier

varsAndType = do
    vs <- identifier `sepBy1` comma
    t <- type'
    return [(v, t) | v <- vs]

structure :: Parser (Identifier, Fields)
structure = do
    "type"
    name <- identifier
    "struct"
    fields <- concat <$> inBraces (varsAndType `sepEndBy` semicolon)
    semicolon
    return (name, fields)

function :: Parser (Identifier, Function)
function = do
    "func"
    name <- identifier
    parameters <- concat <$> inParens (varsAndType `sepEndBy` comma)
    returns <- option [] $ single <$> type' <|> inParens (type' `sepEndBy1` comma)
    body <- block
    semicolon
    return (name, Function{..})

block :: Parser [Statement]
block = inBraces $ skipMany semicolon >> statement `sepEndBy` skipMany1 semicolon

statement = Block <$> block
        <|> ifStatement
        <|> do "var"
               vs <- identifier `sepBy1` comma
               t <- optionMaybe type'
               es <- located $ option [] $ symbol "=" >> expression `sepBy1` comma
               return (Var vs t es)
        <|> ("return" >> Return <$> located (expression `sepBy` comma))
        <|> try (do "for"
                    cond <- option (Bool True :@ nowhere) expression
                    body <- block
                    return (For cond body))
        <|>      do "for"
                    init <- optionMaybe simpleStatement
                    semicolon
                    cond <- expression
                    semicolon
                    post <- option [] $ single <$> simpleStatement
                    body <- block
                    let for = For cond (body ++ post)
                    return $ maybe for (\i -> Block [i, for]) init
        <|> simpleStatement

ifStatement = do
    "if"
    cond <- expression
    yes <- block
    no <- option [] $ "else" >> (block <|> single <$> ifStatement)
    return (If cond yes no)

simpleStatement = try (do vars <- expression `sepBy1` comma
                          symbol "="
                          vals <- located $ expression `sepBy1` comma
                          return (Assign vars vals))
              <|> try (do vars <- identifier `sepBy1` comma
                          symbol ":="
                          vals <- located $ expression `sepBy1` comma
                          return (Var vars Nothing vals))
              <|> try (do e <- expression
                          op <- Increment <$ operator "++" <|> Decrement <$ operator "--"
                          return (op e))
              <|> Expression <$> expression

term :: Parser Expression'
term = Int <$> integer
   <|> String <$> stringLiteral
   <|> Bool True <$ "true"
   <|> Bool False <$ "false"
   <|> Nil <$ "nil"
   <|> inParens (forgetLocation <$> expression)
   <|> try (do f <- Print <$ try fmtPrint <|> Call <$> identifier
               params <- located $ inParens $ expression `sepBy` comma
               return (f params))
   <|> Variable <$> identifier
    where
        fmtPrint = do
            "fmt" :@ _ <- identifier
            symbol "."
            "Print" :@ _ <- identifier
            return "fmt.Print"

dottedTerm = do e <- located term
                foldl (\e m -> Dot e m :@ e `merge` m) e <$> many (symbol "." >> identifier)

expression = buildExpressionParser operators dottedTerm
    where
        operators = [ unary  <$> ["-", "*", "&", "!"]
                    , binary <$> ["*", "/", "%"]
                    , binary <$> ["+", "-"]
                    , binary <$> ["==", "!=", ">", ">=", "<", "<="]
                    , binary <$> ["&&"]
                    , binary <$> ["||"]
                    ]
        unary op = Prefix (do o <- try (operator op)
                              return (\e -> Unary o e :@ o `merge` e))
        binary op = Infix (do o <- try (operator op)
                              return (\e1 e2 -> Binary o e1 e2 :@ e1 `merge` e2))
                          AssocLeft

file :: Parser File
file = do
    whitespace
    "package"; "main" :@ _ <- identifier; semicolon
    fmt <- optionMaybe $ located $ () <$ do "import"; "fmt" <- stringLiteral; semicolon
    let scan = do s <- structure
                  rest <- scan
                  return $ rest { structures = s:structures rest }
           <|> do f <- function
                  rest <- scan
                  return $ rest { functions = f:functions rest }
           <|> File fmt [] [] <$ eof
    scan

parseFile = runParser file initialState
