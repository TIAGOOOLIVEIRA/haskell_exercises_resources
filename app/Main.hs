module Main where

import System.IO
import System.Random
import Lib
import Data

main :: IO ()
main = do 
 gen <- newStdGen
 let filledInGrid = fillInBlanks gen grid
     game = makeGame filledInGrid languages
 hSetBuffering stdout NoBuffering
 playTurn game

playTurn game = do
 putStrLn . formatGame $ game  
 putStrLn "Please enter a word to seek in the list:> "
 word <- getLine
 let newGame = playGame game word
 if completed newGame then
  putStrLn "Congrats :)"
 else
  playTurn newGame 

--putStrLn $ "You entered " ++ word
--let gwc = gridWithCoords grid
--in outputGrid gwc
