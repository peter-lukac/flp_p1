{-# LANGUAGE RecordWildCards #-}

import System.IO
import System.Environment
import Data.List
import Data.Ord

import PLGParser (getPLG, PLG(..), Rule(..))


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-i"] -> do
            putStrLn "-i, change da world"
        ["-i", filename] -> do
            inputData <- readFile filename
            let plg = adjustPLG $ getPLG inputData
            putStr $ show plg
        ["-1"] -> do
            putStrLn "-1, change da world"
        ["-1", filename] -> do
            putStrLn "-1, ok"

adjustPLG :: PLG -> PLG
adjustPLG plg@PLG{..} = PLG nonTerminals terminals start adjustedRules
    where
        adjustedRules = map adjustRule rules

adjustRule :: Rule -> Rule
adjustRule rule@Rule{..} = Rule leftSide rightSide1 rightSide2
    where
        rightSide1 = filter (\x -> (x >= 'a' && x <= 'z') || x == '#') rightSideTerminals
        rightSide2 = filter (\x -> x >= 'A' && x <= 'Z') rightSideTerminals