module PeepholeRefCount where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

-- Three-Address Code (TAC) representation
data Expr = 
         Var String            
         | Const String           
         | Add Expr Expr  
         | Sub Expr Expr  
         deriving (Show, Eq)

data TAC = 
         Assign String Expr     -- x = y
         | New String         -- allocates some memory for x
         deriving (Show, Eq)

data GCAC = 
         Plain TAC
         | Incref String     -- incref x
         | Decref String     -- decref x
         deriving (Show, Eq)

type Alloc = [(String, Int)] -- Variable to number of references (ignoring the actual memory address)


refCount :: [TAC] -> [GCAC]
refCount = refCount' []

refCount' :: Alloc -> [TAC] -> [GCAC]
refCount' _ [] = []

refCount' alloc (New var : rest) = 
    [Plain (New var), Incref var] ++ refCount' alloc' rest
        where
            alloc' = updateAlloc alloc var 1

refCount' alloc (Assign var expr : rest) =
    decrefs ++ [Plain (Assign var expr)] ++ increfs ++ refCount' newAlloc rest
        where
            (decrefs, allocAfterDecref) = handleOverwrite alloc var
            (increfs, newAlloc) = handleExpr allocAfterDecref expr

-- Helper: Update refcount for a variable in Alloc by delta (1 or -1)
updateAlloc :: Alloc -> String -> Int -> Alloc
updateAlloc alloc var delta = update (lookup var alloc)
    where 
        update :: Maybe Int -> Alloc
        update (Just count) = (var, count + delta) : filter ((/= var) . fst) alloc -- list comprehension?
        update Nothing      = (var, delta) : alloc

-- Helper: If 'var' was allocated, decrement its refcount
handleOverwrite :: Alloc -> String -> ([GCAC], Alloc)
handleOverwrite alloc var =
  case lookup var alloc of
    Just count | count > 0 ->
      let newAlloc = updateAlloc alloc var (-1)
      in ([Decref var], newAlloc)
    _ -> ([], alloc)

-- Helper: Increment refcount for vars in 'expr' that are allocated
handleExpr :: Alloc -> Expr -> ([GCAC], Alloc)
handleExpr alloc expr =
  let vars = varsInExpr expr
      (increfs, newAlloc) = foldr f ([], alloc) vars
      f var (gcac, alloc') =
        case lookup var alloc' of
          Just _ -> (Incref var : gcac, updateAlloc alloc' var 1)
          Nothing -> (gcac, alloc')
  in (increfs, newAlloc)

-- recursively find all variables used in the expression
varsInExpr :: Expr -> [String]
varsInExpr (Var v)      = [v]
varsInExpr (Const _)    = []
varsInExpr (Add e1 e2)  = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Sub e1 e2)  = varsInExpr e1 ++ varsInExpr e2
