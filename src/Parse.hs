module Parse where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Int
import Data.Char
import Text.Parsec hiding (digit, hexDigit)
import Text.Parsec.Expr

data Type = Type String
          | Pointer Type
          deriving Show

data File = File { importFmt :: Bool
                 , structures :: [(String, Signature)]
                 , functions :: [(String, Function)]
                 }
          deriving Show

addStructure s f = f { structures = s:structures f }
addFunction fun f = f { functions = fun:functions f }

type Signature = [(String, Type)]

data Function = Function Signature [Type] [Statement]
              deriving Show

data Statement = Expression Expression
               | Increment Expression
               | Decrement Expression
               | Var [String] (Maybe Type) [Expression]
               | Assign [Expression] [Expression]
               | Return [Expression]
               | Block [Statement]
               | If Expression [Statement] [Statement]
               | For Expression [Statement]
               deriving Show

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
                deriving Show

type AutoSemicolon = Bool

type Parser = Parsec String AutoSemicolon

l = flip label

single x = [x]

optionBool p = option False (True <$ p)

lineComment = try (string "//") >> anyChar `manyTill` (() <$ newline <|> eof)
blockComment = try (string "/*") >> anyChar `manyTill` try (string "*/")
whitestuff = () <$ oneOf " \t\r\n" <|> () <$ lineComment <|> () <$ blockComment <?> "whitespace"
whitespace = skipMany whitestuff

lexeme final p = do
    getState >>= guard . not
    l <- p
    line <- sourceLine <$> getPosition
    whitespace
    line' <- sourceLine <$> getPosition
    putState (line' > line && final)
    return l

symbol s = lexeme final $ l (show s) $ string s
    where final = s `elem` [")", "}"]

operator op = lexeme final $ l (show op) $ string op <* notFollowedBy (oneOf "=<>+-*/%&|!")
    where final = op `elem` ["++", "--"]

semicolon = ";" <$ (getState >>= guard >> putState False) <|> symbol ";" <?> "a semicolon"
comma = symbol "," <?> "a comma"

inParens = between (symbol "(") (symbol ")")
inBraces = between (symbol "{") (symbol "}")

alpha = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_' <?> "a letter"

digit = satisfy isDigit <?> "a digit"

word = liftA2 (:) alpha (many (alpha <|> digit))

reserved = ["else", "false", "for", "func", "if", "import", "nil", "package", "return", "struct", "true", "type", "var"]

identifier = lexeme True $ l "an identifier" $ do
    w <- word
    guard (w `notElem` reserved)
    return w

keyword kw = try $ lexeme final $ l (show kw) $ do
    w <- word
    guard (w == kw)
    return w
    where final = kw `elem` ["true", "false", "nil", "return"]

-- TODO: parse negative literals?
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

type_ = (symbol "*" >> Pointer <$> type_) <|> Type <$> identifier

returnType = single <$> type_ <|> inParens (type_ `sepEndBy1` comma)

varsAndType = do
    vs <- identifier `sepBy1` comma
    t <- type_
    return [(v, t) | v <- vs]

structure = do
    keyword "type"
    name <- identifier
    keyword "struct"
    sig <- concat <$> inBraces (varsAndType `sepEndBy` semicolon)
    semicolon
    return (name, sig)

function = do
    keyword "func"
    name <- identifier
    params <- concat <$> inParens (varsAndType `sepEndBy` comma)
    rt <- option [] returnType
    body <- block
    semicolon
    return (name, Function params rt body)

block = inBraces $ skipMany semicolon >> statement `sepEndBy` skipMany1 semicolon

statement = Block <$> block
        <|> ifStatement
        <|> do keyword "var"
               vars <- identifier `sepBy1` comma
               t <- optionMaybe type_
               vals <- option [] $ symbol "=" >> expression `sepBy1` comma
               return (Var vars t vals)
        <|> do keyword "return"
               Return <$> expression `sepBy` comma
        <|> try (do keyword "for"
                    cond <- option (Boolean True) expression
                    body <- block
                    return (For cond body))
        <|>      do keyword "for"
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
    keyword "if"
    cond <- expression
    yes <- block
    no <- option [] $ keyword "else" >> (block <|> single <$> ifStatement)
    return (If cond yes no)

simpleStatement = try (do vars <- expression `sepBy1` comma
                          symbol "="
                          vals <- expression `sepBy1` comma
                          return (Assign vars vals))
              <|> try (do vars <- identifier `sepBy1` comma
                          symbol ":="
                          vals <- expression `sepBy1` comma
                          return (Var vars Nothing vals))
              <|> try (do e <- expression
                          op <- Increment <$ operator "++" <|> Decrement <$ operator "--"
                          return (op e))
              <|> Expression <$> expression

term = Int <$> integer
   <|> String <$> stringLiteral
   <|> Boolean True <$ keyword "true"
   <|> Boolean False <$ keyword "false"
   <|> Nil <$ keyword "nil"
   <|> inParens expression
   <|> try (do f <- Print <$ try fmtPrint <|> Call <$> identifier
               params <- inParens $ expression `sepBy` comma
               return (f params))
   <|> Variable <$> identifier
    where
        fmtPrint = do
            "fmt" <- identifier
            symbol "."
            "Print" <- identifier
            return "fmt.Print"

term' = do e <- term
           foldl Dot e <$> many (symbol "." >> identifier)

expression = buildExpressionParser table term' where
    table = [ unary  <$> ["-", "*", "&", "!"]
            , binary <$> ["*", "/", "%"]
            , binary <$> ["+", "-"]
            , binary <$> ["==", "!=", ">", ">=", "<", "<="]
            , binary <$> ["&&"]
            , binary <$> ["||"]
            ]
    unary op = Prefix (Unary op <$ try (operator op))
    binary op = Infix (Binary op <$ try (operator op)) AssocLeft

file :: Parser File
file = do
    whitespace
    keyword "package"; "main" <- identifier; semicolon
    fmt <- optionBool $ do keyword "import"; "fmt" <- stringLiteral; semicolon
    let scan = do s <- structure
                  addStructure s <$> scan
           <|> do f <- function
                  addFunction f <$> scan
           <|> File fmt [] [] <$ eof
    scan

parseGo = runParser file False
