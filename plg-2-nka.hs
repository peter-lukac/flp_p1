{-# LANGUAGE RecordWildCards #-}

import System.IO
import System.Environment
import Data.List
import Data.Ord
import System.Exit (die)

import PLG2NKA_Data (PLG(..), Rule(..))
import PLGParser (getPLG, adjustPLG, testPLG)

data RuleAndNonTs = RuleAndNonTs [Rule] [String]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-i"] -> do
            putStrLn "-i, change da world"
        ["-i", filename] -> do
            inputData <- readFile filename
            --let plg = adjustPLG $ getPLG inputData
            --putStr $ show plg
            case testPLG inputData of {Left err -> error err; Right plg -> putStr $ show plg}
        ["-1"] -> do
            putStrLn "-1, change da world"
        ["-1", filename] -> do
            inputData <- readFile filename
            case testPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ transformPLG plg


splitRule :: Rule -> [String] -> RuleAndNonTs
-- if rule is A-># then return it
splitRule (Rule a ['#'] c) nonTs = RuleAndNonTs [Rule a ['#'] c] nonTs
-- if rule is A->x then add epsilon rule
splitRule (Rule a (b:[]) "") nonTs = RuleAndNonTs [Rule a [b] newNonT, Rule newNonT ['#'] ""] (concat [nonTs, [newNonT]])
    where
        newNonT = (head a):(show $ length $ filter (\x -> x == (head a)) (map head nonTs))
-- if rule is A->xB then return it
splitRule (Rule a (b:[]) c) nonTs = RuleAndNonTs [Rule a [b] c] nonTs
-- if rule is A->x..xB then split it
splitRule (Rule a (b:bs) c) nonTs = RuleAndNonTs ((Rule a [b] newNonT):newRules) newNonTs
    where
        newNonT = (head a):(show $ length $ filter (\x -> x == (head a)) (map head nonTs))
        RuleAndNonTs newRules newNonTs = splitRule (Rule newNonT bs c) (concat [nonTs, [newNonT]])


transformRules :: [Rule] -> [String] -> RuleAndNonTs
-- transform last rule
transformRules (rule:[]) nonTs = splitRule rule nonTs
-- splitting all rules
transformRules (rule:rules) nonTs = RuleAndNonTs (concat [rules1, rules2]) nonTs2
    where
        RuleAndNonTs rules1 nonTs1 = splitRule rule nonTs
        RuleAndNonTs rules2 nonTs2 = transformRules rules nonTs1

transformPLG :: PLG -> PLG
transformPLG plg@PLG{..} = PLG newNonTs terminals start newRules
    where
        RuleAndNonTs newRules newNonTs = transformRules rules nonTerminals