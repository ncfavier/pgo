import Control.Monad
import Text.Printf
import Text.Parsec
import Text.Parsec.Error
import System.Environment
import System.Exit
import System.FilePath

import AST (Located(..))
import Parse
import Compile

data Stage = Parse | Type | Compile
           deriving (Eq, Ord)

usage = die "usage: pgoc [ --parse-only | --type-only ] FILE"

errorHeader file lineStart lineEnd columnStart columnEnd =
    if lineStart == lineEnd then
        printf "File \"%s\", line %d, characters %d-%d:" file lineStart columnStart columnEnd
    else
        printf "File \"%s\", lines %d-%d, characters %d-%d:" file lineStart lineEnd columnStart columnEnd

parseError err = die $
    errorHeader file line line column column ++
    showErrorMessages "or" "unknown error" "expecting" "unexpected character" "end of file" (errorMessages err)
    where pos = errorPos err; file = sourceName pos; line = sourceLine pos; column = sourceColumn pos

typeError file (e :@ (start, end)) = die $ errorHeader file ls le cs ce ++ "\ntype error: " ++ e
    where [ls, le, cs, ce] = [sourceLine, sourceColumn] <*> [start, end]

main = do
    -- Process command line arguments
    args <- getArgs
    (inputFile, stage) <- maybe usage return $ case args of
        ["--parse-only", inputFile] -> Just (inputFile, Parse)
        ["--type-only", inputFile]  -> Just (inputFile, Type)
        [inputFile]                 -> Just (inputFile, Compile)
        _ -> Nothing
    -- Parse
    input <- readFile inputFile
    f <- either parseError return $ parseFile inputFile input
    when (stage <= Parse) exitSuccess
    -- Compile
    output <- either (typeError inputFile) return $ compileFile f
    when (stage <= Type) exitSuccess
    -- Write
    -- writeFile (inputFile -<.> "s") output
