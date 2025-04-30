import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List ( isPrefixOf )
import Data.Maybe ( isJust )
import Control.Applicative ((<|>))

-- A simple Parser Expression Grammar matcher in Haskell
data Expr = 
    Lit String           -- literal string
    | Var String         -- variable
    | Eps                -- epsilon (nothing)
    | Seq Expr Expr      -- sequence
    | Alt Expr Expr      -- alternative (x or y with left bias)
    | Neg Expr           -- negative lookahead !e
    | Rep0 Expr          -- zero or more repetitions
    | Cls (Set Char)     -- character class 

-- The Following are usually used in PEG but can be made up of other expressions
rep1 :: Expr -> Expr     -- one or more repetitions ()+
rep1 e = Seq e (Rep0 e)

opt :: Expr -> Expr      -- optional ()?
opt e = Alt e Eps

anyChr :: Expr           -- any character
anyChr = Cls (Set.fromList ['\0' .. '\255'])

pos :: Expr -> Expr      -- positive lookahead &e
pos e = Neg (Neg e)

eof :: Expr              -- end of file
eof = Neg anyChr

type PEG = Map String Expr

matches :: String -> PEG -> String -> Bool
matches start peg input = isJust (go (Var start) input) where 
  go :: Expr -> String -> Maybe String
  go (Lit lit) input 
    | isPrefixOf lit input = Just (drop (length lit) input)
    | otherwise            = Nothing
  go (Var v) input = go (peg Map.! v) input
  go Eps input = Just input
  go (Seq e1 e2) input = do -- if e1 matches input, then give the remainder to e2
    input' <- go e1 input
    go e2 input'
  go (Alt e1 e2) input = go e1 input <|> go e2 input
  {-
    This already gives us the left-biased behaviour we need, as it happens:
        Just x <|> _ = Just x
        Nothing <|> mx = mx
  -}
  go (Neg e1) input = case go e1 input of
    Just _ -> Nothing
    Nothing -> Just input
  go (Cls cs) (c:input) | Set.member c cs = Just input
  go (Cls _) _                            = Nothing
  go (Rep0 e) input = let loop = Alt (Seq e loop) Eps in go loop input -- matches * expressions

p :: PEG
p = Map.fromList [
    -- p <- expr !.
    ("p", Seq (Var "expr") eof),
    -- expr <- term ("+" term)*
    ("expr", Seq (Var "term") (Rep0 (Seq (Lit "+") (Var "term")))), 
    -- term <- atom ("*" atom)*
    ("term", Seq (Var "atom") (Rep0 (Seq (Lit "*") (Var "atom")))), 
    -- atom <- [0-9]* / "(" expr ")"
    ("atom", Alt (rep1 (Cls (Set.fromList ['0'..'9']))) 
                 (Seq (Lit "(") (Seq (Var "expr") (Lit ")"))))
    ]

main :: IO ()
main = do
    putStrLn "Enter a string to parse:"
    input <- getLine
    if matches "p" p input
      then putStrLn "The string is valid according to the grammar."
      else putStrLn "The string is invalid according to the grammar."
    -- Example of a valid string for the grammar: 3+5*2
    