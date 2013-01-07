module Main where
import Language.Lua.Parser (fromFile)
import Language.Lua.AST
import Language.Lua.Lint.Lint
import Language.Lua.Lint.Report
import Language.Lua.Show
import Language.Lua.Lint.Env
import Data.List
import Control.Monad
import Data.Either (rights)
import System.Environment (getArgs)

comp e a b  = compare (e a) (e b)
isUnused (Report _ (Danger _)) = True
isUnused _ = False
--main = getArgs >>= mapM_ (\path -> LuaParser.fromFile path >>= putStrLn . either error (unlines . map show . sortBy (comp reportPos) . reports . inspectProgram initialEnv))
--main = getArgs >>= mapM_ (\path -> LuaParser.fromFile path >>= putStrLn . either error (show . length. reports . inspectProgram initialEnv))
main = 
    getArgs >>=  
    mapM fromFile >>=
    return . inspectPrograms . rights >>=
    putStrLn . unlines . map show . sortBy (comp reportPos) . reports
    --foldM (\env path -> LuaParser.fromFile path >>= return . either error (inspectProgram env)) initialEnv >>=
--    putStrLn . unlines . map show . filter isUnused . sortBy (comp reportPos) . reports
    --putStrLn . unlines . map show . sortBy (comp reportPos) . reports
--main = getArgs >>= mapM_ (\path -> LuaParser.fromFile path >>= putStrLn . either error (showLua 0))
