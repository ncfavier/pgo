import Text.Printf
import Text.Parsec
import Text.Parsec.Error
import System.Environment
import System.Exit

import Parser

data Mode = ParseOnly | TypeOnly | Normal

errorHeader f line column = printf "File \"%s\", line %d, characters %d-%d:" f line column column

usage = error "usage: pgoc [ --parse-only | --type-only ] FILE"

main = do
    args <- getArgs
    let (filename, mode) = case args of
            ["--parse-only", f] -> (f, ParseOnly)
            ["--type-only", f] -> (f, TypeOnly)
            [f] -> (f, Normal)
            _ -> usage
    r <- parseGo filename <$> readFile filename
    case r of
        Right s -> return ()
        Left err -> do
            let pos = errorPos err
            die $ errorHeader (sourceName pos) (sourceLine pos) (sourceColumn pos) ++
                showErrorMessages "or" "unknown error" "expecting" "unexpected character" "end of file" (errorMessages err)
