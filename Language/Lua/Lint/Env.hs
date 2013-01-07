module Language.Lua.Lint.Env where
import Control.Monad.ST (ST)
import Data.STRef
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree
import Text.Parsec.Pos (SourcePos)

import Language.Lua.AST
import Language.Lua.Type
import Language.Lua.Semantics
import Language.Lua.Lint.Report

data Env = Env {
    lintConf :: LintConf,
    scopeGraph :: ScopeGraph, 
    depGraph :: DepGraph, 
    scratchNodes :: [G.Node],
    currentScope :: ScopeGraphNode,
    reports :: [Report]}

data LintConf = LintConf { lintConfFunctionThreshold :: Int }

type ScopeGraphNode = G.Node
type ScopeGraph = Data.Graph.Inductive.PatriciaTree.Gr ScopeNode ScopeEdge
type ScopeNode = M.Map ScopeNodeKey ScopeNodeItem 
type ScopeNodeKey = String
type ScopeNodeItem = (DepGraphNode, SourcePos, UsageType, [EvalSpec]) -- (depgraph node, type of variable)
type ScopeEdge = ()

type DepGraphNode = G.Node 
type DepGraph = Data.Graph.Inductive.PatriciaTree.Gr DepNode DepEdge
type DepNode = (Name, (UsageType, LuaType)) -- (scopegraph node, use|define, type)
type DepEdge = ()

class Inspect a where
    inspect :: STRef s Env -> a -> ST s ()
    inspectMany :: STRef s Env -> [a] -> ST s ()
    inspectMany r = mapM_ (inspect r)
-- initializer
scopeNodeEnv = 0 :: G.Node 
scopeNodeGlobal = succ scopeNodeEnv :: G.Node

initialEnv :: Env
initialEnv = Env initialConf initialScopeGraph initialDepGraph [(succ scopeNodeGlobal)..] scopeNodeGlobal []

initialConf :: LintConf
initialConf = LintConf 150

initialScopeGraph :: ScopeGraph
initialScopeGraph = G.mkGraph [(scopeNodeEnv, M.empty), (scopeNodeGlobal, M.empty)] []

initialDepGraph :: DepGraph
initialDepGraph = G.mkGraph [] []

-- conf helper
getConf r = readSTRef r >>= return . lintConf
-- graph helpers: scope graph
readCurrentNode r = readSTRef r >>= return . currentScope

getNodeLabelHere r = readCurrentNode r >>= getNodeLabel r

getNodeLabel r node = readSTRef r >>= \env ->
    return . snd . G.labNode' . fromJust $ fst (G.match node (scopeGraph env))

getNodeParent r node = readSTRef r >>= \env ->
    case G.pre' . fromJust $ fst (G.match node (scopeGraph env)) of
        [] -> return Nothing
        [x] -> return (Just x)
        _  -> error "multiple parents"

modifyNodeLabelHere r f = readCurrentNode r >>= modifyNodeLabel r f

modifyNodeLabel :: STRef s Env -> (ScopeNode -> ScopeNode) -> G.Node -> ST s ()
modifyNodeLabel r f node =
    readSTRef r >>= \env ->
    case G.match node (scopeGraph env) of
        (Just (pre,node,label,suc), rest) -> modifySTRef r $ \env ->
            env {scopeGraph = (pre, node, f label, suc) G.& rest}
        _ -> error "target node not found"

inNewScope :: STRef s Env -> ST s () -> ST s ()
inNewScope r c = pushScope r >> c >> popScope r

pushScope r = 
    createScope r >>= \newNode ->
    modifySTRef r (\env ->
        env { currentScope = newNode, 
              scopeGraph = G.insEdge (currentScope env, newNode, ()) (scopeGraph env)}) >>
    return newNode

popScope r = 
    modifySTRef r $ \env ->
    case G.pre (scopeGraph env) (currentScope env) of
        [parent] -> env { currentScope = parent }
        [] -> error "reverting root"
        _ -> error "multiple parent"

createScope r = readSTRef r >>= \env ->
    let (newNode:restNodes) = scratchNodes env in
        writeSTRef r (env {
            scopeGraph = G.insNode (newNode, M.empty) (scopeGraph env), 
            scratchNodes = restNodes}) >>
        return newNode
-- graph helpers: dependent graph
addDepNode :: STRef s Env -> Name -> UsageType -> EvalSpec -> ST s DepGraphNode
addDepNode r name usage spec = -- TODO: maybe spec is not necessary
    readSTRef r >>= \env ->
    let (newNode:restNodes) = scratchNodes env in
        writeSTRef r (env { 
            depGraph = G.insNode (newNode, (name, (usage, fst spec))) (depGraph env),
            scratchNodes = restNodes}) >>
        return newNode

linkDepNode :: STRef s Env -> G.Node -> Name -> EvalSpec -> ST s ()
linkDepNode r src name spec = addDepNode r name Use spec >>= \newNode ->
    modifySTRef r $ \env ->
        env { depGraph = G.insEdge (src, newNode, ()) (depGraph env)}
