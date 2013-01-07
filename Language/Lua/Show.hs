module Language.Lua.Show
( showLua
) where
import Text.Parsec (SourcePos)
import Data.List (intercalate)
import Language.Lua.AST
import Language.Lua.Symbol (Intrinsic(..))

parens :: String -> String
parens x = "(" ++ x ++ ")"

csl :: [String] -> String
csl xs = intercalate ", " xs

eol :: String
eol = "\n"

indent :: Int -> String
indent level = (iterate (' ':) " ") !! (level * 2) 

class ShowLua a where
    showLua :: Int -> a -> String

instance ShowLua LuaProgram where
    showLua level (LuaProgram path block) = "-- " ++ path ++ eol ++ showLua level block

instance ShowLua Block where
    showLua level (Block stmts last) = eol ++ unlines (map (showLua level) stmts) ++ maybe "" (\stmt -> showLua level stmt) last

instance ShowLua Function where
    showLua level (Function _ args block _) = 
        "(" ++ intercalate ", " (map snd args) ++ ")" ++ showLua (level + 1) block ++ replicate (level * 2) ' ' ++ "end"

instance ShowLua Statement where
    showLua level stmt = wrap $ case stmt of 
        Assign _ lhs rhs -> 
            unwords [showCSL lhs,"=",showCSL rhs]
        LocalDef _ lhs rhs -> 
            unwords ["local",csl (map snd lhs),"=",showCSL rhs]
        Do _ block -> 
            "do" ++ showLua'' block ++ indent' ++ "end"
        For _ var idx1 idx2 idx3 block -> 
            unwords ["for",snd var,"=",csl [showLua' idx1, showLua' idx2], "do"] ++ maybe "" ((", " ++) . showLua') idx3 ++ showLua'' block ++ indent' ++ "end"
        ForEach _ defs exps block -> 
            unwords ["for",csl (map snd defs),"in",showCSL exps, "do"] ++ showLua'' block ++ indent' ++ "end"
        While _ cond block -> 
            unwords ["while", showLua' cond, "do"] ++ showLua'' block ++ indent' ++ "end"
        Repeat _ block cond -> 
            "repeat" ++ showLua'' block ++ indent' ++ unwords ["until", showLua' cond]
        If _ _ _ _ ->  showIfBlock False stmt 
        StatementCall func -> showLua' func
        FunctionDef _ exp func  ->
            unwords ["function", showLua' exp]++ showLua' func
        LocalFunctionDef _ def func ->
            unwords ["local function", snd def] ++ showLua' func
        FunctionDefSelf _ exp method func  ->
            unwords ["function", showLua' exp ++ ":" ++ snd method] ++ showLua' func
        EmptyStatement _ -> ";"
        where
        showIfBlock continued stmt = (if continued then wrap else id) $ case stmt of 
            If _ exp block Nothing -> 
                unwords[if continued then "else" else "if",showLua' exp, "then"] ++ showLua'' block ++ indent' ++ "end"
            If _ exp block (Just (Do _ elseStmt))-> 
                unwords [(if continued then "elseif" else "if"), showLua' exp, "then"] ++ showLua'' block ++ indent' ++ "else" ++ showLua'' elseStmt ++ indent' ++ "end"
            If _ exp block (Just next@(If _ _ _ _)) -> 
                unwords [if continued then "elseif" else "if", showLua' exp, "then"] ++ showLua'' block ++ showIfBlock True next

        showLua' :: (ShowLua a) => a -> String
        showLua' = showLua level
        showLua'' = showLua (level + 1)
        indent' = indent level
        showCSL xs = csl (map showLua' xs)
        wrap l = indent' ++ l 

instance ShowLua LastStatement where
    showLua level exp = case exp of
        Return _ exps -> indent level ++ unwords ["return", intercalate ", " $ map (showLua level) exps] ++ eol
        Break _ -> indent level ++ "break" ++ eol

instance ShowLua FunctionCall where
    showLua level stmt = case stmt of 
        Call _ exp args -> showLua' exp ++ "(" ++ showArgs args ++ ")"
        CallSelf _ exp method args -> showLua' exp ++ ":" ++ snd method ++ "(" ++ showArgs args ++ ")"
        where
        showLua' = showLua level
        showArgs args = csl (map showLua' args)

instance ShowLua Exp where
    showLua level exp = case exp of
        Literal _ v -> showLua level v
        Fetch (_, v) -> v
        Index _ e i -> showLua' e ++ "[" ++ showLua' i ++ "]"
        Parens e -> "(" ++ showLua' e ++ ")"
        Dot _ e i -> showLua' e ++ "." ++ snd i
        ExpCall (Call _ e args) -> showLua' e ++ "(" ++ showArgs args ++ ")"
        ExpCall (CallSelf _ e m args) -> showLua' e ++ ":" ++ snd m ++ "(" ++ showArgs args ++ ")"
        Binop _ intr l r -> unwords [showLua' l, showLua level intr, showLua' r]
        Unop _ intr e -> showLua' intr ++ showLua' e
        Nop -> "--[[nop]]"
        where
        showLua' :: (ShowLua a) => a -> String
        showLua' = showLua level
        showArgs args = csl (map showLua' args)

instance ShowLua TypedValue where
    showLua level value = case value of
        TypedString DoubleQuoted str -> "\"" ++ str ++ "\""
        TypedString SingleQuoted str -> "'" ++ str ++ "'"
        TypedString MultiLined str -> "[[" ++ str ++ "]]"
        TypedInt int -> show int
        TypedReal double -> show double
        TypedBool bool -> if bool then "true" else "false"
        TypedVararg -> "..."
        TypedFunction func -> "function" ++ showLua level func 
        TypedUserdata -> "<user_data>"
        TypedThread -> "<thread>"
        TypedNil -> "nil"
        TypedTable [] -> "{}"
        TypedTable items -> "{\n" ++ unlines [indent (level + 1) ++ showLua (level + 1) s ++ "," | s <- items] ++ indent level ++ "}"

instance ShowLua TableItem where
    showLua level item = case item of
        TableItemValue _ exp -> showLua level exp
        TableItemKeyValue _ key value -> unwords [key, "=", showLua level value]
        TableItemValueValue _ key value -> unwords ["["++ showLua level key ++"]", "=", showLua level value]

instance ShowLua Intrinsic where
    showLua _ (Intrinsic s) = s
