module Language.Lua.AST where
import Text.Parsec (SourcePos)
import Language.Lua.Symbol (Intrinsic)

data LuaProgram = LuaProgram FilePath Block deriving(Eq, Ord, Show)

data Block = Block [Statement] (Maybe LastStatement) deriving(Eq, Ord, Show)

data Function = Function SourcePos [Name] Block SourcePos deriving(Eq, Ord, Show)

data Statement =
   Assign SourcePos [Exp] [Exp]
   |LocalDef SourcePos [Name] [Exp]
   |Do SourcePos Block
   |If SourcePos Exp Block (Maybe Statement)
   |For SourcePos Name Exp Exp (Maybe Exp) Block
   |ForEach SourcePos [Name] [Exp] Block
   |While SourcePos Exp Block
   |Repeat SourcePos Block Exp
   |StatementCall FunctionCall
   |LocalFunctionDef SourcePos Name Function
   |FunctionDef SourcePos Exp Function
   |FunctionDefSelf SourcePos Exp Name Function
   |EmptyStatement SourcePos
   deriving(Eq, Ord, Show)

type Name = (SourcePos, String)

data LastStatement =
   Return SourcePos [Exp]
   |Break SourcePos
   deriving(Eq, Ord, Show)

data FunctionCall =
   Call SourcePos Exp [Exp]
   |CallSelf SourcePos Exp Name [Exp]
   deriving(Eq, Ord, Show)

data Exp = 
   Literal SourcePos TypedValue 
   |Fetch Name
   |Index SourcePos Exp Exp 
   |Parens Exp
   |Dot SourcePos Exp Name
   |ExpCall FunctionCall
   |Binop SourcePos Intrinsic Exp Exp
   |Unop SourcePos Intrinsic Exp
   |Nop
   deriving(Eq, Ord, Show)

data TypedValue = 
    TypedString { typedStringSyntax :: StringSyntax, typedStringValue :: String }
    |TypedInt Integer 
    |TypedReal Double 
    |TypedBool Bool
    |TypedTable [TableItem]
    |TypedVararg
    |TypedFunction Function
    |TypedUserdata
    |TypedThread
    |TypedNil
    deriving(Eq, Ord, Show)

data StringSyntax = DoubleQuoted | SingleQuoted | MultiLined deriving (Eq, Ord, Show)

data TableItem =
   TableItemValue SourcePos Exp
   |TableItemKeyValue SourcePos String Exp
   |TableItemValueValue SourcePos Exp Exp
   deriving(Eq, Ord, Show)

data TableItemSyntax = TableItemSyntaxValue | TableItemSyntaxKeyValue | TableItemSyntaxValueValue deriving(Eq, Ord, Show)

-- XXX: uncomfortable
getTableItemSyntax (TableItemValue _ _) = TableItemSyntaxValue
getTableItemSyntax (TableItemKeyValue _ _ _) = TableItemSyntaxKeyValue
getTableItemSyntax (TableItemValueValue _ _ _) = TableItemSyntaxValueValue
