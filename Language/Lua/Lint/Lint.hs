module Language.Lua.Lint.Lint where
import Text.Parsec.Pos (SourcePos, sourceLine)
import Control.Monad.ST (runST, ST)
import Control.Monad (when, unless, sequence_)
import Control.Applicative
import Data.STRef
import Data.Function (on)
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree
import Language.Lua.AST
import Language.Lua.Type
import Language.Lua.Symbol
import Language.Lua.Semantics
import Language.Lua.Parser (fromFile)
import Language.Lua.Lint.Env
import Language.Lua.Lint.Rules
import Language.Lua.Lint.Report
-- report
report :: STRef s Env -> SourcePos -> ReportDetail -> ST s ()
report r pos detail = modifySTRef r $ \env ->
    env { reports = (Report pos detail):reports env }
-- definition lookup
searchRecHere :: STRef s Env -> Name -> ST s (Maybe (ScopeGraphNode, ScopeNodeItem))
searchRecHere r name = readCurrentNode r >>= searchRec r name

-- DRY
searchThere :: STRef s Env -> Name -> ScopeGraphNode -> ST s (Maybe (ScopeGraphNode, ScopeNodeItem))
searchThere r name@(pos,def) node =
    getNodeLabel r node >>= \vmap ->
    case M.lookup def vmap of
        Nothing -> return Nothing
        Just x -> return (Just (node,x))

searchRec :: STRef s Env -> Name -> G.Node -> ST s (Maybe (Int, ScopeNodeItem))
searchRec r name@(pos,def) node =
    searchThere r name node >>= 
    maybe 
        (getNodeParent r node >>= maybe (return Nothing) (searchRec r name))
        (return . Just)
-- fetch
fetch :: STRef s Env -> Name -> ST s EvalSpec
fetch r name@(pos,_) =
    readCurrentNode r >>= \currentNode ->
    searchRecHere r name >>= \result ->
    case result of
        Nothing ->
            report r pos (Danger (UseOfUndefinedVariable name)) >>
            return (LuaTypeNil, Just TypedNil)
        Just (definedNode, (depNode, _, _, (spec@(tp,val):_))) ->
            linkDepNode r depNode name spec >>
            if currentNode == definedNode then
                return spec
            else
                return (tp, Nothing) -- conservative constant propagation

-- TODO fetch as
fetchTableItem :: STRef s Env -> Name -> ScopeGraphNode -> ST s EvalSpec
fetchTableItem r name@(pos,_) node  =
    searchThere r name node >>= \result ->
    case result of
        Nothing ->
            report r pos (Danger (UseOfUndefinedTableIndex name)) >>
            return (LuaTypeNil, Just TypedNil)
        Just (_, (depNode, _, _, (spec:_))) ->
            linkDepNode r depNode name spec >>
            return spec
-- DRY
fetchModule :: STRef s Env -> Name -> ST s ()
fetchModule r name@(pos,_) =
    searchThere r name scopeNodeEnv >>= \result ->
    case result of
        Nothing -> report r pos (Danger (UseOfUnknownModule name))
        Just (_, (depNode, _, _, (spec:_))) -> linkDepNode r depNode name spec

defineLocal  :: STRef s Env -> Name -> EvalSpec -> ST s ()
defineLocal r name@(pos,def) spec =
    readCurrentNode r >>= \currentNode ->
    searchRecHere r name >>= \var ->
    addDepNode r name DefineLocal spec >>= \depNode ->
    modifyNodeLabelHere r (M.insert def (depNode, pos, DefineLocal, [spec])) >> -- type is wip
    case var of
        Nothing -> return ()
        Just (node,(_, defPos, _, _)) ->
            if node == scopeNodeGlobal then
                report r pos (Wrong (MaskingGlobalDefinitionByLocal def defPos))
            else if node == currentNode then
                report r pos (Wrong (MaskingPreviousLocalDefinition def defPos))
            else
                return ()

defineTableItemThere :: STRef s Env -> Name -> EvalSpec -> ScopeGraphNode -> ST s DepGraphNode
defineTableItemThere r name@(pos,def) spec node =
    searchThere r name node >>= \var ->
    addDepNode r name DefineTableItem spec >>= \depNode ->
    modifyNodeLabel r (M.insert def (depNode, pos, DefineTableItem, [spec])) node >> -- type is wip
    (case var of
        Nothing -> return ()
        Just (node,(_, defPos, _, _)) ->
            report r pos (Wrong (MaskingPreviousTableItem def defPos))) >>
    return depNode

defineTableItemHere :: STRef s Env -> Name -> EvalSpec -> ST s DepGraphNode
defineTableItemHere r name@(pos,def) spec =
    readCurrentNode r >>= defineTableItemThere r name spec

defineGlobal :: STRef s Env -> Name -> EvalSpec -> ST s ()
defineGlobal r name@(pos,def) spec = addDepNode r name DefineGlobal spec >>= \depNode ->
    modifyNodeLabel r (M.insert def (depNode, pos, DefineGlobal, [spec])) scopeNodeGlobal

defineModule :: STRef s Env -> Name -> EvalSpec -> ST s ()
defineModule r name@(pos,def) spec =
    searchThere r name scopeNodeEnv >>= \mod ->
    addDepNode r name DefineModule spec >>= \depNode ->
    modifyNodeLabel r (M.insert def (depNode, pos, DefineModule, [spec])) scopeNodeEnv >>
    case mod of
        Nothing -> return ()
        Just (node,(_, defPos, _, _)) -> report r pos (Danger (DuplicateModuleDefinition (defPos, def)))

defineLoopIndex :: STRef s Env -> Name -> EvalSpec -> ST s ()
defineLoopIndex r name@(pos,def) spec = addDepNode r name DefineIndex spec >>= \depNode ->
    modifyNodeLabelHere r (M.insert def (depNode, pos, DefineIndex, [spec]))

unlessM mb ms = mb >>= \b -> unless b ms
-- rules for statements
hookStmt :: STRef s Env -> Statement -> ST s Bool
hookStmt r stmt = case stmt of
    (StatementCall
        (Call _
            (Fetch (_, "export"))
            [Literal pos (TypedString _ x), exp])) ->
                eval r exp >>=
                defineGlobal r (pos,x) >>
                return True
    (StatementCall
        (Call _
            (Fetch (_, "define"))
            [Literal pos (TypedString _ x), exp])) ->
                eval r exp >>=
                defineModule r (pos,x) >>
                return True
    (StatementCall
        (Call _
            (Fetch (_, "rawset"))
            [Fetch (_, "_G"), Literal pos (TypedString _ x), exp])) ->
                eval r exp >>=
                defineGlobal r (pos,x) >>
                return True
    (StatementCall
        (Call _
            (Fetch (_, "rawset"))
            [Fetch (_, "GLOBAL"), Literal pos (TypedString _ x), exp])) ->
                eval r exp >>=
                defineModule r (pos,x) >>
                return True
    _ -> return False

checkStmt :: STRef s Env -> Statement -> ST s ()
checkStmt r stmt = case stmt of
    (Do pos (Block [] Nothing)) -> 
        report r pos (Deprecated EmptyBlock)
    (For _ _ _ _ (Just (Literal pos (TypedInt 1))) _) ->
        report r pos (Deprecated RedundantStepIndex)
    (If _ (Literal pos val)
        _
        _) -> report r pos (Deprecated (ConstantConditionalExpression val))
    (If pos _
        (Block [] Nothing)
        _) -> report r pos (Deprecated EmptyConditionalBlock)
    (If pos exp1
        (Block [If _ exp2 _ Nothing] Nothing)
        _) -> report r pos (Deprecated (RedundantConditionalBlock exp1 exp2))
    _ -> return ()
-- rules for expressions
checkExp r exp = case exp of
    (ExpCall
        (Call _
            (Fetch (_, "import"))
            [Literal pos (TypedString _ x)])) ->
        fetchModule r (pos,x)
    (ExpCall
        (Call _
            (Fetch (pos, "rawequal"))
            [Literal _ (TypedString _ x), exp])) ->
        report r pos (Deprecated RawEqualForString)
    (ExpCall
        (Call _
            (Fetch (pos, "rawequal"))
            [_, Literal _ (TypedString _ x)])) ->
        report r pos (Deprecated RawEqualForString)
    (Literal pos (TypedTable es@(e1:e2:_))) ->
        let (x:xs) = map getTableItemSyntax es in
            if all (x ==) xs then
                return ()
            else
                report r pos (Deprecated MultipleStyleInTable)
    (Dot pos (Fetch (_, "GLOBAL")) exp) ->
        report r pos (Deprecated DirectModuleSpaceAccess)
    _ -> return ()
-- helper
inspectNewScope :: Inspect a => STRef s Env -> a -> ST s ()
inspectNewScope r a = inNewScope r (inspect r a)

assignSpec :: STRef s Env -> ScopeGraphNode -> String -> EvalSpec -> ST s ()
assignSpec r node def spec =
     modifyNodeLabel r (M.update (\(a,b,c,d@(dh:_)) -> if dh == spec then Just (a,b,c,d) else Just (a,b,c,spec:d)) def) node   

assign :: STRef s Env -> Exp -> EvalSpec -> ST s ()
assign r lhs@(Fetch name@(pos,def)) spec =
    searchRecHere r name >>= \result ->
    case result of
        Nothing ->
            defineGlobal r name spec >>
            report r pos (Wrong (ImplicitGlobalDefinition def))
        Just (node, (_, defPos, defType, _)) -> 
            assignSpec r node def spec >>
            when (defType == DefineIndex)
                 (report r pos (Deprecated (ModifyingLoopIndex def defPos)))
assign r lhs@(Index idxPos obj idx) spec =
    eval r obj >>= \(objType, objVal) ->
    eval r idx >>= \(idxType, idxVal) ->
        case objType of
            LuaTypeNil ->
                report r idxPos (Danger NilTableIndex)
            tp ->
                if isIndexibleType tp then
                    case (tp, idxType, idxVal) of
                        (LuaTypeTable node, LuaTypeString, Just (TypedString _ key)) ->
                            searchThere r (idxPos, key) node >>= 
                            maybe (defineTableItemThere r (idxPos, key) spec node >> return ()) 
                                  (const (assignSpec r node key spec))
                        (LuaTypeUnknown, LuaTypeString, Just (TypedString _ key)) ->
                            createUnknownTable r [] >>= \(LuaTypeUnknownTable node, _) ->
                            defineTableItemThere r (idxPos,key) spec node >>= \depNode ->
                            linkDepNode r depNode (idxPos,key) spec -- exclude unknown table item from unused list
                        (LuaTypeUnknownTable node, LuaTypeString, Just (TypedString _ key)) ->
                            defineTableItemThere r (idxPos,key) spec node >>= \depNode ->
                            linkDepNode r depNode (idxPos,key) spec -- exclude unknown table item from unused list
                        (a,b,c) -> return () -- could not tract
                else
                    report r idxPos (Danger (IndexingIllegalType tp))
assign r lhs@(Dot dotPos obj name@(pos,def)) spec = 
     assign r (Index dotPos obj (Literal pos (TypedString SingleQuoted def))) spec

instance Inspect LuaProgram where
    inspect r (LuaProgram path block) = inspectNewScope r block

instance Inspect Block where
    inspect r (Block list last) = inspectMany r list >> maybe (return ()) (inspect r) last

instance Inspect Function where
    inspect r (Function pos args block endPos) =
        sequence_ (zipWith (defineLocal r) args (repeat unknownEvalSpec)) >>
        inspect r block >>
        getConf r >>= \conf ->
        let actual = sourceLine endPos - sourceLine pos
            limit = lintConfFunctionThreshold conf in
            if  actual >= limit then
                report r pos (Deprecated (TooLongFunction actual limit))
            else
                return ()

instance Inspect LastStatement where
    inspect r stmt = case stmt of
        Return pos exps -> evalMany r exps >> return ()
        Break pos -> return ()

expectType :: STRef s Env -> SourcePos -> LuaType -> EvalSpec -> ST s (Maybe EvalSpec)
expectType r pos expect spec@(actual, _)
    | expect == actual = return (Just spec)
    | actual == LuaTypeUnknown = return (Just spec)
    | otherwise = 
        report r pos (Danger (TypeMismatch expect actual)) >>
        return Nothing

--useFunction
expectFunction r pos spec@(tp,_) =
    unless ((tp == LuaTypeFunction) || (tp == LuaTypeUnknown))
           (report r pos (Danger (CallingNonFunctionalType tp)))

evalMany r = mapM (eval r)

instance Inspect Statement where
    inspect r stmt = checkStmt r stmt >> unlessM (hookStmt r stmt) (case stmt of
        Assign pos lhs rhs ->
            evalMany r rhs >>= \specs ->
            sequence_ (zipWith (assign r) lhs (specs ++ repeat nilEvalSpec))
        LocalDef pos defs rhs ->
            evalMany r rhs >>= \specs ->
            sequence_ (zipWith (defineLocal r) defs (specs ++ repeat nilEvalSpec))
        Do pos block ->
            inspectNewScope r block
        If pos cond list opt ->
            eval r cond >>= \(_, mlit) -> 
            maybe (return ()) (report r pos . Deprecated . ConstantConditionalExpression) mlit >>
            inspectNewScope r list >>
            maybe (return ()) (inspect r) opt
        For pos def idx1 idx2 idx3 block ->
            inNewScope r $
                eval r idx1 >>= \spec1 ->
                expectType r pos LuaTypeNumber spec1 >>
                eval r idx2 >>= \spec2 ->
                expectType r pos LuaTypeNumber spec2 >>
                maybe (return nilEvalSpec) 
                      (\exp -> eval r exp >>= \spec -> 
                       expectType r pos LuaTypeNumber spec >> 
                       return spec) idx3 >>
                defineLoopIndex r def spec1 >>
                inspect r block
        ForEach pos defs exps block ->
            inNewScope r $
                evalMany r exps >>= \specs ->
                sequence_ (zipWith (defineLoopIndex r) defs (specs ++ repeat nilEvalSpec)) >> -- todo set correct types
                inspect r block
        While pos cond block ->
            eval r cond >>
            inspectNewScope r block
        Repeat pos block cond ->
            inNewScope r $
                inspect r block >>
                eval r cond >> return ()
        StatementCall (Call pos func args) ->
            eval r func >>=
            expectFunction r pos >>
            evalMany r args >> return ()
        StatementCall (CallSelf pos obj method@(idxPos, def) args) ->
            eval r obj >>= \lspec ->
            tryTableIndex r idxPos lspec (toStringEvalSpec def) >>=
            expectFunction r pos >>
            evalMany r args >> return ()
        LocalFunctionDef pos def func ->
            defineLocal r def (LuaTypeFunction, Just (TypedFunction func)) >>
            inspectNewScope r func
        FunctionDef pos (Fetch def) func ->
            defineGlobal r def (LuaTypeFunction, Just (TypedFunction func)) >>
            inspectNewScope r func
        FunctionDef pos exp func ->
            eval r exp >> -- TODO expect table type
            inspectNewScope r func
        FunctionDefSelf pos obj method func ->
            eval r obj >> -- TODO expect table type
            inNewScope r (
                defineLocal r (pos, "self") unknownEvalSpec >>
                inspect r func)
        EmptyStatement pos ->
            report r pos (Deprecated UseOfEmptyStatement)
            )

instance Inspect TableItem where
    inspect r item = case item of
        TableItemValue pos exp -> eval r exp >> return ()
        TableItemKeyValue pos key exp ->
            eval r exp >>= defineTableItemHere r (pos,key) >> return ()
        TableItemValueValue pos lhs rhs ->
            eval r rhs >>= \rspec ->
            eval r lhs >>= \lspec ->
                case lspec of
                    (LuaTypeString, Just (TypedString _ def)) ->
                        defineTableItemHere r (pos,def) rspec >> return ()
                    _ -> return ()

createTableBase :: STRef s Env -> (G.Node -> LuaType) -> [TableItem] -> ST s EvalSpec
createTableBase r dt items =
    pushScope r >>= \node ->
    inspectMany r items >>
    popScope r >>
    return (dt node, Just (TypedTable items))

createTable :: STRef s Env -> [TableItem] -> ST s EvalSpec
createTable r items = createTableBase r (LuaTypeTable) items

createUnknownTable :: STRef s Env -> [TableItem] -> ST s EvalSpec
createUnknownTable r items = createTableBase r (LuaTypeUnknownTable) items

tryTableIndex :: STRef s Env -> SourcePos -> EvalSpec -> EvalSpec -> ST s EvalSpec
tryTableIndex r pos (tp,_) (idxType, idxVal) 
    | tp == LuaTypeNil =
        report r pos (Danger NilTableIndex) >>
        return nilEvalSpec
    | isIndexibleType tp =
        case (tp, idxType, idxVal) of
            (LuaTypeTable node, LuaTypeString, Just (TypedString _ key)) ->
                fetchTableItem r (pos, key) node
            _ -> return (tp, Nothing)
    | otherwise = 
        report r pos (Danger (IndexingIllegalType tp)) >>
        return (tp, Nothing)

eval ::  STRef s Env -> Exp -> ST s EvalSpec
eval r exp = checkExp r exp >> case exp of
    Literal pos lit ->
        case lit of
            TypedFunction func ->
                inspectNewScope r func >>
                return (getType exp, Just lit)
            TypedTable items -> createTable r items
            TypedVararg -> fetch r (pos, "...")
            _ -> return (getType exp, Just lit)
    Index pos obj index ->
        eval r obj >>= \lspec -> 
        eval r index >>= \rspec ->
        tryTableIndex r pos lspec rspec
    Parens exp -> eval r exp
    Fetch def -> fetch r def
    Dot pos obj name@(dotPos, def) ->
        eval r (Index pos obj (Literal dotPos (TypedString SingleQuoted def)))
    ExpCall (Call pos func args) ->
        eval r func >>= 
        expectFunction r pos >>
        evalMany r args >>
        return unknownEvalSpec
    ExpCall (CallSelf pos obj method@(idxPos,def) args) ->
        eval r obj >>= \lspec ->
        tryTableIndex r idxPos lspec (toStringEvalSpec def) >>=
        expectFunction r pos >>
        evalMany r args >>
        return unknownEvalSpec
    Binop pos intr lhs rhs -> 
        eval r lhs >>= \lspec -> 
        eval r rhs >>= \rspec ->
        return (head (propagate (intrSemantics intr) [lspec, rspec]))
    Unop pos intr exp -> 
        eval r exp >>= \spec ->
        return (head (propagate (intrSemantics intr) [spec]))
    Nop -> return nilEvalSpec -- not reach

mergeResults :: Env -> Env
mergeResults env = runST $ newSTRef env >>= \r -> scanUnused r >> readSTRef r
    where
    scanUnused r =
        readSTRef r >>= \env ->
        mapM_ (reportUnused r (depGraph env)) (G.labNodes (depGraph env))
        where
        reportUnused _ _ (_, (_, (Use, _))) = return ()
        reportUnused _ _ (_, (_, (DefineIndex, _))) = return ()
        reportUnused r gr (node, (name@(pos,def), (usage, tp)))
            | G.suc gr node == [] =
                when (notElem def ["_", "setup", "execute", "leave"])
                     (report r pos (Deprecated (UnUsedVariable usage def)))
            | otherwise = return ()

inspectPrograms :: [FilePath] -> Env -> IO Env
inspectPrograms [] env = return (mergeResults env)
inspectPrograms (f:fs) env =
    fromFile f >>= \result ->
        case result of
            Right prog -> inspectPrograms fs (runST $ newSTRef env >>= \r ->
                inspect r prog >> readSTRef r)
            Left s -> error s

inspectFiles :: [FilePath] -> IO Env
inspectFiles progs =
  inspectPrograms progs (initialEnv { currentScope = scopeNodeGlobal })
