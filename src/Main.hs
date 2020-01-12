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

usage :: IO a
usage = die "usage: pgoc [ --parse-only | --type-only ] FILE"

errorHeader :: SourcePos -> SourcePos -> String
errorHeader start end
    | ls == le  = printf "File \"%s\", line %d, characters %d-%d:" f ls cs ce
    | otherwise = printf "File \"%s\", lines %d-%d, characters %d-%d:" f ls le cs ce
    where
        [ls, le] = sourceLine   <$> [start, end]
        [cs, ce] = sourceColumn <$> [start, end]
        f        = sourceName start

parseError :: ParseError -> IO a
parseError err = die $
    errorHeader pos pos ++
    showErrorMessages "or" "unknown error" "expecting" "unexpected character" "end of file" (errorMessages err)
    where pos = errorPos err

typeError :: TypeError -> IO a
typeError (err :@ Just (start, end)) = die $
    errorHeader start end ++ "\ntype error: " ++ err
typeError (err :@ Nothing) = die $ "type error: " ++ err

main :: IO ()
main = do
    -- Process command line arguments
    args <- getArgs
    (f, stage) <- maybe usage return $ case args of
        ["--parse-only", f] -> Just (f, Parse)
        ["--type-only", f]  -> Just (f, Type)
        [f]                 -> Just (f, Compile)
        _                   -> Nothing
    -- Parse
    input <- readFile f
    file <- either parseError return $ parseFile f input
    when (stage <= Parse) exitSuccess
    -- Compile
    output <- either typeError return $ compileFile file
    when (stage <= Type) exitSuccess
    -- Output
    writeFile (f -<.> "s") output
