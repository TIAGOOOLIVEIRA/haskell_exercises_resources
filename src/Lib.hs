module Lib
    ( someFunc
     ,getLines
     ,formatGameGrid
     ,fillInBlanks
     ,findWordInLine
     ,skew
     ,someString
     ,outputGrid
     ,formatGrid
     ,findWord
     ,findWords 
     ,zipOverGridWith
     ,zipOverGrid
     ,coordsGrid
     ,gridWithCoords
     ,cell2char
     ,Cell(Cell, Indent)
     ,Game(gameGrid, gameWords)
     ,makeGame
     ,totalWords
     ,score
     ,formatGame
     ,completed
     ,makeRandomGrid
     ,playGame
     ,findWordInCellLinePrefix
   ) where

import System.Random
import Data.Char (toLower)
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad
import qualified Data.Map as M

data Game = Game {
              gameGrid :: Grid Cell,
              gameWords :: M.Map String (Maybe [Cell])
            }
            deriving Show

data Cell = Cell (Integer, Integer) Char 
          | Indent
            deriving (Eq, Ord, Show)

--type Grid = [String]
type Grid a = [[a]]

-- `.` and `$` pipelining functions
totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

makeRandomGrid gen =
 let (gen1, gen2) = split gen 
     row = randomRs ('T', 'I') gen1
 in  row : makeRandomGrid gen2

--"stateful" 
--makeRandomGrid gen = 
-- let row = randomRs ('T', 'I') gen
-- in row : makeRandomGrid gen

formatGameGrid :: Game -> String
formatGameGrid game = 
 let grid = gameGrid game
     dict = gameWords game :: M.Map String (Maybe [Cell])
     cellSet = concat . catMaybes . M.elems $ dict
     formatCell cell = 
      let char = cell2char cell
      in if cell `elem` cellSet then char else toLower char
     charGrid = mapOverGrid cell2char grid
 in unlines charGrid

fillInBlanks gen grid = 
 let r = makeRandomGrid gen
     fill '_' r = r
     fill c _   = c
 in zipOverGridWith fill grid r

formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\n"
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)

--pattern matching @
--formatGame game@(Game grid dict) = formatGrid grid

--pattern matching
playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word = 
 let grid = gameGrid game
     foundWord = findWord grid word
 in case foundWord of
      Nothing -> game
      Just cs ->
       let dict = gameWords game
           newDict = M.insert word foundWord dict
       in game { gameWords = newDict }

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

--pattern matching
--or using curly bracet in the type Game. Enables to remove from the export list
--gameGrid grid _ = grid
--gameWords _ words = words

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
 let rows = map repeat [0..]
     cols = repeat [0..]
 in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

--pattern matching
cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
 let lines = getLines grid
     foundWords = map (findWordInLine word) lines
 in listToMaybe (catMaybes foundWords)


findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
 let foundWords = map (findWord grid) words
 in catMaybes foundWords

diagonalize :: Grid Cell -> Grid Cell
diagonalize grid = transpose (skew grid)

--diagonalize grid = (transpose . skew) grid

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
 let horizontal = grid
     vertical = transpose grid
     diagonal1 = diagonalize grid
     diagonal2 = diagonalize (map reverse grid)
     lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
 in lines ++ (map reverse lines) 

--recursive
skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew(map indent ls)
 where indent line = Indent : line

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
 let found = findWordInCellLinePrefix [] word line 
 in case found of
  Nothing -> findWordInLine word (tail line)
  cs@(Just _) -> cs 

--findWordInLine = undefined --isInfixOf 

--recursive function, accumulator, pattern matching, prepend with `:`
findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char  c
 = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

--grid = [ "__C________R___"
--       , "__SI________U__"
--       , "__HASKELL____B_"
--       , "__A__A_____S__Y"
--       , "__R___B___C____"
--       , "__PHP____H_____"
--       , "____S_LREP_____"
--       , "____I__M_Y__L__"
--       , "____L_E__T_O___"
--       , "_________HB____"
--       , "_________O_____"
--       , "________CN_____"
--       ]

--languages = [ "BASIC"
--            , "COBOL"
--            , "CSHARP"
--            , "HASKELL"
--            , "LISP"
--            , "PERL"
--            , "PHP"
--            , "PYTHON"
--            , "RUBY"
--            , "SCHEME"
--            ]

