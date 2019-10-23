import Text.Printf
import Text.Parsec
import Text.Parsec.Error
import System.Environment

import Parser

errorHeader f line column = printf "File \"%s\", line %d, characters %d-%d:" f line column column

main = do
    [filename] <- getArgs
    r <- parseGo filename <$> readFile filename
    case r of
        Right s -> print s
        Left err -> do
            let pos = errorPos err
            putStrLn $ errorHeader (sourceName pos) (sourceLine pos) (sourceColumn pos) ++
                showErrorMessages "or" "unknown error" "expecting" "unexpected character" "end of file" (errorMessages err)
