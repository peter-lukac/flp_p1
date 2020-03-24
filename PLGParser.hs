-- plg parser

{-# LANGUAGE RecordWildCards #-}

module PLGParser  where

import System.IO
import System.Environment
import Data.List
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.Ord
import Text.Parsec (parse, newline, string, char, sepBy1, endBy, many1, upper, lower, sepBy, count, choice)
import Text.Parsec.String (Parser)
import PLG2NKA_Data (PLG(..), Rule(..))

-- Typ výsledku nebo text případné chyby
type Err = Either String

testPLG :: String -> Either String PLG
testPLG = validate . adjustPLG <=< left show . parse parsePLG ""

getPLG :: String -> PLG
getPLG input = case parse parsePLG "" input of
            {   Left err -> error $ show err
            ;   Right ans -> ans
            }


-- main parser function
parsePLG :: Parser PLG
parsePLG = PLG <$> nonTerminalList <* newline
               <*> terminalList    <* newline
               <*> count 1 upper   <* newline
               <*> ruleList

-- parseRule list parsing, one for each line
ruleList :: Parser [Rule]
ruleList = endBy parseRule newline

-- single parseRule parsing
parseRule :: Parser Rule
parseRule = Rule <$> count 1 upper <* string "->" <*> choice [string "#", rightSide] <*> string ""


-- parses non-empty right side of the rule
rightSide :: Parser String
rightSide = many1 lower <> choice [count 1 upper, string ""]

-- non-terminal symbols parsing
nonTerminalList :: Parser [String]
nonTerminalList = sepBy1 (count 1 upper) comma

-- terminal symbols parsing
terminalList :: Parser [Char]
terminalList = sepBy lower comma

-- "," symbol parser
comma :: Parser Char
comma = char ','


validate :: PLG -> Err PLG
validate plg@PLG{..} = if ok then Right plg else Left "invalid PLG"
    where
        ok = elem start nonTerminals
            && all ((`elem` nonTerminals) . leftSide) rules
            && all ((all (`elem` '#':terminals)) . rightSideTerminals) rules
            && all ((`elem` "":nonTerminals) . rightSideNonTerminal) rules


adjustPLG :: PLG -> PLG
adjustPLG plg@PLG{..} = PLG nonTerminals terminals start adjustedRules
    where
        adjustedRules = map adjustRule rules

adjustRule :: Rule -> Rule
adjustRule rule@Rule{..} = Rule leftSide rightSide1 rightSide2
    where
        rightSide1 = filter (\x -> (x >= 'a' && x <= 'z') || x == '#') rightSideTerminals
        rightSide2 = filter (\x -> x >= 'A' && x <= 'Z') rightSideTerminals