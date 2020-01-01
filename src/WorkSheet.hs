import Control.Monad (guard)
import Data

main :: IO ()
main = putStrLn (greet "World")

data Compass = North | East | South | West

instance Show Compass where 
 show North = "North"
 show East = "East"
 show South = "South"
 show West = "West"

instance Eq Compass where
 North == North = True

data Bussola = Norte | Leste | Sul | Oeste
 deriving (Eq, Ord, Enum, Show)

data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)

--partial function, expressions
calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)

--pattern matching
--type signature
newHead :: [a] -> a
newHead [] = error "empty list"
newHead [a] = a
newHead [a,b,c] = b
-- :info []
-- single until plural value, prepend for any size
newHead (x:xs) = x

greeting = "Hello by var"
greet who = greeting ++ ", " ++ who

add :: Int -> Int -> Int
add a b = a + b

--List monad notation
mapped = do
 i <- [0..]
 return (i * 2)

filtered = do
 i <- [0..]
 guard (div2 i)
 return i

mappedAndFiltered = do
 i <- [0..]
 guard (div2 i)
 return (i + 1)

coords2 = do
 row <- [0..7]
 return $ do
   col <- [0..7]
   return (row, col)

coords3 = do
 row <- [0..7]
 do
  col <- [0..7]
  return (row, col)

-- variant of outputGrid, for arbitrary Show-able structures
og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

-- check if divisible by 2
div2 x = x `mod` 2 == 0

cols = repeat [0..]
rows = map repeat [0..]
coordsInf = zipOverGrid rows cols

repeat8 = take 8 . repeat
cols8 = repeat8 [0..7]
rows8 = map repeat8 [0..7]


zipOverGrid = zipWith zip
grid8 = zipOverGrid rows8 cols8

--zipOverGridWith f a b = (zipWith (zipWith f)) a b
zipOverGridWith = zipWith . zipWith
