module ReferenceCounting where
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

-- =====================
-- Reference Counting is the simplest form of garbage collection, we increment
--  the reference count of a memory block every time that it's used, and decrement
--  it when it's value is overwritten. When this count reaches 0, we can free the memory.
-- =====================
-- What happens when we have cyclic dataStructures?
--  ie. inside of an allocated  memory block is a reference to another
--  allocated memory block, and so on?
-- =====================

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
         | Free String       -- free x
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

-- if the variable being defined already points to allocated memory, then decrement
handleOverwrite :: Alloc -> String -> ([GCAC], Alloc)
handleOverwrite alloc var = handleOverwrite' (lookup var alloc)
    where
        handleOverwrite' :: Maybe Int -> ([GCAC], Alloc)
        handleOverwrite' (Just count)
            | count > 1 = ([Decref var], newAlloc)
            | count == 1 = ([Decref var, Free var], newAlloc)
            | otherwise = ([], alloc) -- shouldnt happen
                 where 
                    newAlloc = updateAlloc alloc var (-1)

        handleOverwrite' Nothing = ([], alloc)

-- if theyve alrady been defined and point to allocated memory, then increment
-- NOTE: we're not handling the case where a varaible is used before it is defined
handleExpr :: Alloc -> Expr -> ([GCAC], Alloc)
handleExpr alloc expr = (increfs, newAlloc)
    where
        vars = varsInExpr expr
        (increfs, newAlloc) = foldr generateIncs ([], alloc) vars
        -- let's go through the list of variables, see if they are allocations,
        -- and if they are, add increment instructions to the list
        generateIncs var (gcac, alloc') = gen (lookup var alloc')
          where
            gen :: Maybe Int -> ([GCAC], Alloc)
            gen (Just _) = (Incref var : gcac, updateAlloc alloc' var 1)
            gen Nothing = (gcac, alloc')


-- recursively find all variables used in the expression
varsInExpr :: Expr -> [String]
varsInExpr (Var v)      = [v]
varsInExpr (Const _)    = []
varsInExpr (Add e1 e2)  = varsInExpr e1 ++ varsInExpr e2
varsInExpr (Sub e1 e2)  = varsInExpr e1 ++ varsInExpr e2
