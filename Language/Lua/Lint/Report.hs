{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Language.Lua.Lint.Report where
import Data.List (intercalate)
import Language.Lua.AST
import Language.Lua.Show
import Language.Lua.Type
import Text.Parsec.Pos (SourcePos(..), sourceName, sourceLine, sourceColumn)

-- XXX: should not be here.. should be AST or so
data UsageType = 
    Use 
    | DefineLocal 
    | DefineGlobal 
    | DefineIndex 
    | DefineModule 
    | DefineTableItem
    deriving(Eq, Ord, Show)

data Report = Report {
    reportPos :: SourcePos, 
    reportDetail ::ReportDetail
} deriving(Eq, Ord)

instance Show Report where
    show repo = 
        let pos = reportPos repo in
             intercalate ":" [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos,  show (reportDetail repo)]

data ReportDetail = 
    Danger DangerDetail 
    |Wrong WrongDetail 
    |Deprecated DeprecatedDetail 
    |Notice NoticeDetail 
    |Hint HintDetail 
    deriving(Eq, Ord, Show)

data HintDetail = HintDetail deriving(Show, Eq, Ord)
data NoticeDetail = NoticeDetail deriving(Show, Eq, Ord)
data WrongDetail = 
    DuplicatedDefinitionInSameScope | ArgumentNumberMismatch 
    |AssignmentNumberMismatch 
    |AssignedByDiffrentType | ImplicitGlobalDefinition String
    |MaskingGlobalDefinitionByLocal String SourcePos
    |MaskingPreviousTableItem String SourcePos
    |MaskingPreviousLocalDefinition String SourcePos
    |RedefiningLocalFunction
    |OverridingGlobalFunction
    |CamelCased
    deriving(Eq, Ord)

instance Show WrongDetail where
    show detail = case detail of
        ImplicitGlobalDefinition def ->
            unwords ["global variable", show def, "is defined"]
        MaskingPreviousTableItem def pos ->
            unwords ["table item", show def, "masks previous definition at", show pos]
        MaskingGlobalDefinitionByLocal def pos ->
            unwords ["local variable", show def, "masks global definition at", show pos]
        MaskingPreviousLocalDefinition def pos ->
            unwords ["local variable", show def, "masks previous local definition at", show pos]

data DeprecatedDetail = 
    EmptyBlock 
    |ConstantConditionalExpression TypedValue
    |InferredConstantConditionalExpression TypedValue
    |MultipleStyleInTable 
    |RedundantStepIndex
    |UnUsedVariable UsageType String 
    |UseOfBlacklisted 
    |EmptyConditionalBlock
    |RedundantConditionalBlock Exp Exp
    |DirectModuleSpaceAccess
    |RawEqualForString
    |TooLongFunction Int Int -- actual, limit
    |ModifyingLoopIndex String SourcePos
    |UseOfEmptyStatement
     deriving(Eq, Ord)

instance Show DeprecatedDetail where
    show detail = case detail of
        TooLongFunction actual limit ->
            unwords ["function is too long:", show actual, "lines exceeds limit", show limit]
        RedundantStepIndex ->
            "step index 1 in for statement is redundant"
        RedundantConditionalBlock exp1 exp2 -> 
            unwords ["redundant if expression. why not", showLua 0 (Parens exp1), "and", showLua 0 (Parens exp2), "?"]
        MultipleStyleInTable -> 
            "table item definitions are syntactically inconsistent"
        EmptyBlock-> 
            "do/else block is empty"
        RawEqualForString -> 
            "do not use rawequal for string: use == instead"
        EmptyConditionalBlock -> 
            "if/elseif block is empty"
        ConstantConditionalExpression val ->
            unwords ["literal",showLua 0 val,"is used in conditional expression"]
        InferredConstantConditionalExpression val ->
            unwords ["literal",showLua 0 val,"used in conditional expression (inferred)"]
        DirectModuleSpaceAccess ->
            "module space is accessed directly"
        UnUsedVariable usage def -> 
            unwords [showUsage usage,  show def, "is not used"]
        ModifyingLoopIndex def pos ->
            unwords ["modifies loop index", show def, "defined at", show pos]

data DangerDetail = 
    UseOfUndefinedVariable Name 
    | UseOfUnknownModule Name 
    | IndexingIllegalType LuaType 
    | CallingNonFunctionalType LuaType
    | UseOfUndefinedTableIndex Name
    | TypeMismatch LuaType LuaType
    | NilTableIndex | NilTableCount 
    | DuplicateModuleDefinition Name
    | IntrinsicTypeMismatch deriving(Eq, Ord)

instance Show DangerDetail where
    show detail = case detail of
        CallingNonFunctionalType t ->
            unwords ["calling non-functional object of type", show t]
        TypeMismatch expect actual ->
            unwords ["type mismatch:","expected:", show expect,"inferred:", show actual]
        NilTableIndex -> 
            "indexing table is probably nil"
        UseOfUndefinedTableIndex (_,def) ->
            unwords ["table key", show def, "is not defined explicitly"]
        IndexingIllegalType t ->
            unwords ["indexing", show t, "type"]
        UseOfUndefinedVariable (_,def) -> 
            unwords ["undefined variable", show def, "is used"]
        UseOfUnknownModule (_,def) -> 
            unwords ["unknown module", show def, "is imported"]
        DuplicateModuleDefinition (pos,def) -> 
            unwords ["module", show def, "is already defined at", show pos]

showUsage :: UsageType -> String
showUsage Use = "use"
showUsage DefineLocal = "local variable"
showUsage DefineGlobal = "global variable"
showUsage DefineIndex = "loop index"
showUsage DefineTableItem = "table item"
showUsage DefineModule = "module"

