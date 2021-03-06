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
    stats <- runStats config text
    let printStat (w, cnt) = print (w ++ ": " ++ show cnt)
    mapM_ printStat (Map.toAscList stats)

type WordStatStateStack = StateT WordStatistics IO
type WordStatStack a = ReaderT Config WordStatStateStack a

runStats :: Config -> String -> IO WordStatistics
runStats config text = do
    let runTopReaderMonad = runReaderT (calculateStats text) config
    evalStateT runTopReaderMonad Map.empty

runIO :: IO a -> WordStatStack a
runIO = lift . lift

-- Statistics calculation logic
calculateStats :: String -> WordStatStack WordStatistics
calculateStats txt = do
    wordTokens <- tokenize txt
    lift $ collectStats wordTokens

-- Tokenizer, will normalize text and break it into words
tokenize :: String -> WordStatStack [String]
tokenize txt = do
    Config ignore norm <- ask
    let normalize ch = if isAlpha ch then ch else ' '
    let transform1 = if ignore then map toLower else id
    let transform2 = if norm then map normalize else id

    runIO $ print ("Ignoring case: " ++ show ignore)
    runIO $ print ("Normalize: " ++ show norm)

    return . words . transform2 . transform1 $ txt

-- Takes words and counts them using dictionary as statistics data structure
collectStats :: [String] -> WordStatStateStack WordStatistics
collectStats ws = do
    lift $ print $ "Words: " ++ show ws
    mapM_ (modify . countWord) ws
    get

-- Counts single word and updates dictionary
countWord :: String -> WordStatistics -> WordStatistics
countWord w stat = Map.insertWith (+) w 1 stat

data Config = Config { caseIgnoring :: Bool, normalization :: Bool }

type WordStatistics = Map.Map String Int

