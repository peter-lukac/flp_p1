-- FLP - Projekt 1
-- File: PLGParser.hs
-- Author: Peter Lukáč - xlukac11
-- March 2020
-- file containing functions for parsing and checking validity of the grammar

{-# LANGUAGE RecordWildCards #-}

module PLGParser  where

import Control.Applicative ((<$>), (<*>), (<*))
import Data.List
import Control.Arrow (left)
import Control.Monad ((<=<))
import Text.Parsec (parse, newline, string, char, sepBy1, endBy, many1, upper, lower, count,
                    choice)
import Text.Parsec.String (Parser)
import PLG2NKA_Data (PLG(..), Rule(..))


-- takes input string and returns parssed PLG or error string
getPLG :: String -> Either String PLG
getPLG = validate <=< left show . parse parsePLG ""


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
parseRule = Rule <$> count 1 upper <* string "->"
                    <*> choice [string "#", many1 lower]
                    <*> choice [count 1 upper, string ""]



-- non-terminal symbols parsing
nonTerminalList :: Parser [String]
nonTerminalList = sepBy1 (count 1 upper) comma


-- terminal symbols parsing
terminalList :: Parser [Char]
terminalList = sepBy1 lower comma


-- "," symbol parser
comma :: Parser Char
comma = char ','


-- validation of the grammar, returns valid grammar or error string
validate :: PLG -> Either String PLG
validate plg@PLG{..} = if ok then Right plg else Left "invalid PLG"
    where
        ok = elem start nonTerminals
            && all ((`elem` nonTerminals) . leftSide) rules
            && all ((all (`elem` '#':terminals)) . rightSideTerminals) rules
            && all ((`elem` "":nonTerminals) . rightSideNonTerminal) rules
            && all (\x -> length x == 1 ) (map (`elemIndices` nonTerminals) nonTerminals)
            && all (\x -> length x == 1 ) (map (`elemIndices` terminals) terminals)
            && all (\(Rule _ a b) -> (a=="#"&&b=="") || (a/="#") ) rules