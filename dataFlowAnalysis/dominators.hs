import Data.List ( (\\), nub, union )
import GHC.OldList (intersect)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadPrec (look)

data Node = Node
    { nodeId :: Id
    , nodeDefs :: DefSet
    , nodeUses :: UseSet
    , nodeSuccs :: SuccSet
    , nodePreds :: PredSet
    }

type CFG = [Node]
type Id = String
type DefId = String
type DefSet = [DefId]
type UseSet = [DefId]
type PredSet = [Id]
type SuccSet = [Id]

--helper funcions
bigCap :: [[Id]] -> [Id] -- ^ intersection of all sets
bigCap [] = []
bigCap sets = foldr1 intersect sets

untilConverges (a:b:rest) | a == b = a -- stops when the consecutive entries are equal
untilConverges (a:b:rest) = untilConverges (b:rest)

nodesOf :: CFG -> [Id]
nodesOf = map nodeId

retrieve :: Id -> [(Id,[Id])] -> [Id]
retrieve id ds = fromJust (lookup id ds)

getNode :: CFG -> Id -> Node
getNode cfg id = head [n | n <- cfg, nodeId n == id]

-- fills in the table of dominators
-- 1. each node dominates itself
-- 2. node a dominates node b if you can only 'get to node b through node a'
dominatorsOf :: CFG -> [(Id,[Id])]
dominatorsOf cfg = untilConverges (iterate updateDs initialDs)
    where
        initialDs :: [(Id,[Id])] -- everyone starts dominating everyone else
        initialDs = [ (n, nodesOf cfg) | n <- nodesOf cfg]

        updateDs :: [(Id,[Id])] -> [(Id,[Id])]
        updateDs ds = map (updateD ds) ds

        updateD :: [(Id,[Id])] -> (Id,[Id]) -> (Id,[Id])
        updateD ds_sofar (id, ds) = (id, [id] `union` bigCap [retrieve p ds_sofar | p <- nodePredsOf id])

        nodePredsOf id = nodePreds (getNode cfg id)

-- finds edges in the graph between two nodes such that
-- 1. the first node is a predecessor of the second node
-- 2. the second node dominates the first node
-- This is called a back edge and means that there is a natural loop in our code
backEdges :: CFG -> [(Id,Id)]
backEdges cfg = [ (n,h) | n <- nodesOf cfg, h <- nodesOf cfg, n /= h, flowedge n h, h `dominates` n]
    where
        dominators = dominatorsOf cfg
        a `dominates` b = a `elem` retrieve b dominators
        flowedge a b = a `elem` nodePreds (getNode cfg b)

