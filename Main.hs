module Main where
import Language.Lua.Parser (fromFile)
import Language.Lua.Lint.Env (reports)
import Language.Lua.Lint.Lint (inspectFiles)
import Language.Lua.Lint.Report (reportPos)
import Data.List (sortBy)
import Data.Either (rights)
import System.Environment (getArgs)

comp e a b  = compare (e a) (e b)

main =
    getArgs >>=
    inspectFiles >>=
    mapM_ (putStrLn . show) . sortBy (comp reportPos) . reports
