module Language.Lua.Symbol where

data Intrinsic = Intrinsic String
  deriving(Eq, Ord, Show)

binops :: [Either Intrinsic Intrinsic]
binops = 
 [Left (Intrinsic ".."), Left (Intrinsic "or"), Left (Intrinsic "and"),
  Left (Intrinsic "<="), Left (Intrinsic "<"), Left (Intrinsic ">="),
  Left (Intrinsic ">"), Left (Intrinsic "=="), Left (Intrinsic "~="),
  Left (Intrinsic "+"), Left (Intrinsic "-"), Left (Intrinsic "/"),
  Left (Intrinsic "%"), Left (Intrinsic "*")]

unops :: [Intrinsic]
unops = [Intrinsic "-", Intrinsic "not", Intrinsic "#"]

stringOps = ["and","or","not"]

reservedWords = 
   ["and","break","do","else","elseif","end","false","for","function","if",
    "in","local","nil","not","or", "repeat","return","then","true","until","while"]
