import Control.Monad
import Text.Printf
import Text.Parsec
import Text.Parsec.Error
import System.Environment
import System.Exit

import Parse
import Type
import Compile

data Stage = Parse | Type | Compile
           deriving (Eq, Ord)

usage = die "usage: pgoc [ --parse-only | --type-only ] FILE"

parseError err = die $
    printf "File \"%s\", line %d, characters %d-%d:" file line column column ++
    showErrorMessages "or" "unknown error" "expecting" "unexpected character" "end of file" (errorMessages err)
    where pos = errorPos err; file = sourceName pos; line = sourceLine pos; column = sourceColumn pos

typeError err = die "type error"

stripSuffix s [] = []
stripSuffix s l@(x:xs) | l == s    = []
                       | otherwise = x:stripSuffix s xs

main = do
    args <- getArgs
    (inputFile, stage) <- maybe usage return $ case args of
        ["--parse-only", inputFile] -> Just (inputFile, Parse)
        ["--type-only", inputFile] -> Just (inputFile, Type)
        [inputFile] -> Just (inputFile, Compile)
        _ -> Nothing

    f <- either parseError return . parseGo inputFile =<< readFile inputFile
    when (stage <= Parse) exitSuccess

    f' <- either typeError return (typeGo f)
    when (stage <= Type) exitSuccess

    let outputFile = stripSuffix ".go" inputFile ++ ".s"
    writeFile outputFile (compileGo f')
