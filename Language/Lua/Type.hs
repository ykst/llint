module Language.Lua.Type where
import Language.Lua.AST
import qualified Data.Graph.Inductive.Graph as G

data LuaType = 
    LuaTypeString
    |LuaTypeNumber
    |LuaTypeNil
    |LuaTypeFunction
    |LuaTypeBool
    |LuaTypeTable G.Node -- XXX
    |LuaTypeUnknownTable G.Node -- XXX
    |LuaTypeUser
    |LuaTypeThread
    |LuaTypeUnknown
    deriving(Eq, Ord, Show)

isIndexibleType :: LuaType -> Bool
isIndexibleType t = case t of
    LuaTypeTable _ -> True
    LuaTypeString -> True
    LuaTypeUnknown -> True
    LuaTypeUnknownTable _ -> True
    _ -> False

getType :: Exp -> LuaType
getType exp@(Literal pos x) = case x of
    TypedString _ _ -> LuaTypeString
    TypedInt _ -> LuaTypeNumber
    TypedReal _ -> LuaTypeNumber
    TypedNil -> LuaTypeNil
    TypedFunction _ -> LuaTypeFunction
    TypedBool _ -> LuaTypeBool
    TypedUserdata -> LuaTypeUser
    _ -> LuaTypeUnknown
getType exp = LuaTypeUnknown

