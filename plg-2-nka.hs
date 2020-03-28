-- FLP - Projekt 1
-- File: plg-2-nka.hs
-- Author: Peter Lukáč - xlukac11
-- March 2020
-- main file containing main function and functions for grammar conversion

{-# LANGUAGE RecordWildCards #-}

module Main(main) where

import System.IO
import System.Environment
import Data.List

import PLG2NKA_Data (PLG(..), Rule(..), Transition(..), NKA(..))
import PLGParser (getPLG, adjustPLG)


-- main function executes essential functions and prints output
main :: IO ()
main = do
    args <- getArgs
    let (action, filename) = parseArgs args

    inputData <- if filename == ""
        then
            getContents
        else
            readFile filename

    case action of
        "-i" -> do
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show plg
        "-1" -> do
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ transformPLG plg
        "-2" -> do
            case getPLG inputData of
                Left err -> error err
                Right plg -> putStr $ show $ makeNKA $ transformPLG plg


-- parsing arguments
parseArgs :: [String] -> (String, String)
parseArgs [action]
    | action =="-i" = (action, "")
    | action =="-1" = (action, "")
    | action =="-2" = (action, "")
    | otherwise = error "Usage: plg-2-nka -i|-1|-2 [input_file]"

parseArgs [action, filename]
    | action =="-i" = (action, filename)
    | action =="-1" = (action, filename)
    | action =="-2" = (action, filename)
    | otherwise = error "Usage: plg-2-nka -i|-1|-2 [input_file]"

parseArgs _ = error "Usage: plg-2-nka -i|-1|-2 [input_file]"


-- helper structure that is used to return value
data RuleAndNonTs = RuleAndNonTs [Rule] [String]


-- takes any rule and changes it into A->xB or A->#, also adds new non-terminals based on that
splitRule :: Rule -> [String] -> RuleAndNonTs
-- if rule is A-># then return it
splitRule (Rule a "#" "") nonTs = RuleAndNonTs [Rule a "#" ""] nonTs
-- if rule is A->x then add epsilon rule
splitRule (Rule a (b:[]) "") nonTs = RuleAndNonTs newRules (concat [nonTs, [newNonT]])
    where
        newNonT = (head a):(show $ length $ filter (\x -> x == (head a)) (map head nonTs))
        newRules = [Rule a [b] newNonT, Rule newNonT ['#'] ""]
-- if rule is A->xB then return it
splitRule (Rule a (b:[]) c) nonTs = RuleAndNonTs [Rule a [b] c] nonTs
-- if rule is A->x..xB then split it
splitRule (Rule a (b:bs) c) nonTs = RuleAndNonTs ((Rule a [b] newNonT):newRules) newNonTs
    where
        newNonT = (head a):(show $ length $ filter (\x -> x == (head a)) (map head nonTs))
        RuleAndNonTs newRules newNonTs = splitRule (Rule newNonT bs c) (concat [nonTs, [newNonT]])


-- changes all rules into A->xB or A->#
transformRules :: [Rule] -> [String] -> RuleAndNonTs
-- transform last rule
transformRules (rule:[]) nonTs = splitRule rule nonTs
-- transforming all rules
transformRules (rule:rules) nonTs = RuleAndNonTs (concat [rules1, rules2]) nonTs2
    where
        RuleAndNonTs rules1 nonTs1 = splitRule rule nonTs
        RuleAndNonTs rules2 nonTs2 = transformRules rules nonTs1


-- takes PLG and tranforms it into PLG with A->xB or A-># rules
transformPLG :: PLG -> PLG
transformPLG plg@PLG{..} = PLG newNonTs terminals start newRules
    where
        RuleAndNonTs newRules newNonTs = transformRules rules nonTerminals


-- always returns index of the string in array of strings
certainIndex :: String -> [String] -> Int
certainIndex e list = case elemIndex e list of { Just x -> x; Nothing -> 0 }


-- convert PLG rule into NKA transition
rule2Transition :: [String] -> Rule -> Transition
rule2Transition nonTs (Rule a (b:[]) c) = Transition (certainIndex a nonTs)b(certainIndex c nonTs)


-- convert PLG to NKA
makeNKA :: PLG -> NKA
makeNKA plg@PLG{..} = NKA states startState endStates transitions
    where
        states = [0..(length nonTerminals) - 1]
        startState = certainIndex start nonTerminals
        endStates = findIndices (`elem` (map (\(Rule x _ _) -> x) 
                                (filter (\(Rule _ x _) -> x == "#") rules))) nonTerminals
        transitions = map (rule2Transition nonTerminals) (filter (\(Rule _ x _) -> x /= "#") rules)