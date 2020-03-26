{-# LANGUAGE RecordWildCards #-}

import System.IO
import System.Environment
import Data.List
import Data.Ord
import System.Exit (die)

import PLG2NKA_Data (PLG(..), Rule(..), Transition(..), NKA(..))
import PLGParser (getPLG, adjustPLG)

data RuleAndNonTs = RuleAndNonTs [Rule] [String]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-i"] -> do
            inputData <- getContents
            case getPLG inputData of {Left err -> error err; Right plg -> putStr $ show plg}
        ["-i", filename] -> do
            inputData <- readFile filename
            case getPLG inputData of {Left err -> error err; Right plg -> putStr $ show plg}
        ["-1"] -> do
            inputData <- getContents
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ transformPLG plg
        ["-1", filename] -> do
            inputData <- readFile filename
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ transformPLG plg
        ["-2", filename] -> do
            inputData <- readFile filename
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ makeNKA $ transformPLG plg
        ["-2"] -> do
            inputData <- getContents
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ makeNKA $ transformPLG plg
        _   -> putStrLn "Usage: plg-2-nka -i|-1|-2 [input_file]"


splitRule :: Rule -> [String] -> RuleAndNonTs
-- if rule is A-># then return it
splitRule (Rule a "#" "") nonTs = RuleAndNonTs [Rule a "#" ""] nonTs
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


certainIndex :: String -> [String] -> Int
certainIndex e list = case elemIndex e list of { Just x -> x; Nothing -> 0 }


rule2Transition :: [String] -> Rule -> Transition
rule2Transition nonTs (Rule a (b:[]) c) = Transition (certainIndex a nonTs) b (certainIndex c nonTs)

makeNKA :: PLG -> NKA
makeNKA plg@PLG{..} = NKA states startState endStates transitions
    where
        states = [0..(length nonTerminals) - 1]
        startState = certainIndex start nonTerminals
        endStates = findIndices (`elem` (map (\(Rule x _ _) -> x) (filter (\(Rule _ x _) -> x == "#") rules))) nonTerminals
        transitions = map (rule2Transition nonTerminals) (filter (\(Rule _ x _) -> x /= "#") rules)