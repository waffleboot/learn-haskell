module Main where

import Lib
import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)
import Control.Monad.State
import Control.Monad.Reader

-- The entry point to the program
main = do
    let text = "To be, or not to be: that is the question."
    let config = Config True True
    let stats  = runReader (calculateStats text) config
    let printStat (w, cnt) = print (w ++ ": " ++ show cnt)
    mapM_ printStat (Map.toAscList stats) 

-- Statistics calculation logic
calculateStats :: String -> Reader Config WordStatistics
calculateStats txt = do
    wordTokens <- tokenize txt
    return $ execState (collectStats wordTokens) Map.empty

-- Tokenizer, will normalize text and break it into words
tokenize :: String -> Reader Config [String]
tokenize txt = do
    Config ignore norm <- ask
    let normalize ch = if isAlpha ch then ch else ' '
    let transform1 = if ignore then map toLower else id
    let transform2 = if norm then map normalize else id
    return . words . transform2 . transform1 $ txt

-- Takes words and counts them using dictionary as statistics data structure
collectStats :: [String] -> State WordStatistics ()
collectStats ws = mapM_ (modify . countWord) ws

-- Counts single word and updates dictionary
countWord :: String -> WordStatistics -> WordStatistics
countWord w stat = Map.insertWith (+) w 1 stat

data Config = Config { caseIgnoring :: Bool, normalization :: Bool }

type WordStatistics = Map.Map String Int

