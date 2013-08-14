module Language.Lua.Symbol where

data Intrinsic = Intrinsic String
  deriving(Eq, Ord, Show)

-- http://www.lua.org/manual/5.2/manual.html 3.4.7
binops :: [Either Intrinsic Intrinsic]
binops = 
 [Left (Intrinsic "or"), Left (Intrinsic "and"),
  Left (Intrinsic "<"), Left (Intrinsic ">"), Left (Intrinsic "<="), Left (Intrinsic ">="),
  Left (Intrinsic "~="),Left (Intrinsic "=="),
  Right (Intrinsic ".."),
  Left (Intrinsic "+"), Left (Intrinsic "-"), Left (Intrinsic "*"),
  Left (Intrinsic "/"), Left (Intrinsic "%"),
  Right (Intrinsic "^")]

unops :: [Intrinsic]
unops = [Intrinsic "-", Intrinsic "not", Intrinsic "#"]

stringOps = ["and","or","not"]

reservedWords =
   ["and","break","do","else","elseif","end","false","for","function","if",
    "in","local","nil","not","or", "repeat","return","then","true","until","while"]
