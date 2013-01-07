module Language.Lua.Semantics(
    EvalSpec,
    nilEvalSpec,
    unknownEvalSpec,
    toStringEvalSpec,
    OpSemantics(..),
    propagate,
    intrSemantics
) where

import Control.Applicative
import Language.Lua.AST
import Language.Lua.Type
import Language.Lua.Symbol

type EvalSpec = (LuaType, Maybe TypedValue)
nilEvalSpec = (LuaTypeNil, Just TypedNil) :: EvalSpec
unknownEvalSpec = (LuaTypeUnknown, Nothing) :: EvalSpec
toStringEvalSpec s = (LuaTypeString, Just (TypedString SingleQuoted s)) :: EvalSpec

getString (TypedString _ s) = Just s
getString _ = Nothing

data OpSemantics = OpSemantics { 
    minimumArgNum :: Int, 
    argTypes :: [LuaType], 
    minimumReturnNum :: Int,
    returnTypes :: [LuaType],
    propagateFunc :: [Maybe TypedValue] -> Maybe [TypedValue]
}

propagate :: OpSemantics -> [EvalSpec] -> [EvalSpec]
propagate sem args =
    zip (returnTypes sem) $ 
        let fun = propagateFunc sem in
            maybe (repeat Nothing) (map Just) (fun (map snd args))

intrSemantics :: Intrinsic -> OpSemantics
intrSemantics (Intrinsic "..") =
    OpSemantics 2 [LuaTypeString, LuaTypeString] 1 [LuaTypeString] (\args -> case args of
            (Just a1:Just a2:_) -> return <$> (TypedString SingleQuoted <$> ((++) <$> getString a1 <*> getString a2))
            _ -> Nothing)
intrSemantics _ = OpSemantics 0 [] 0 [LuaTypeUnknown] (const Nothing)
