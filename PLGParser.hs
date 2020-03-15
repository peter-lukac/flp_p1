-- plg parser

{-# LANGUAGE RecordWildCards #-}

module PLGParser  where

import System.IO
import System.Environment
import Data.List
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.Ord
import Text.Parsec (Parsec, ParseError, parse,
        newline, alphaNum, string, char, satisfy, sepBy1, endBy, many1, upper, lower, endOfLine, sepBy, count, skipMany, try, lookAhead, many, choice, between, option)
import Text.Parsec.String (Parser)

-- Typ výsledku nebo text případné chyby
type Err = Either String

data Rule = Rule
    { leftSide :: String
    , rightSideTerminals :: [Char]
    , rightSideNonTerminal :: String
    } deriving (Eq)
instance Show Rule where
    show (Rule l r1 r2) = l ++ "->" ++ r1 ++ r2

data PLG = PLG
    { nonTerminals :: [String]
    , terminals :: [Char]
    , start :: String
    , rules :: [Rule]
    } deriving (Eq)

instance Show PLG where
    show (PLG n t s r) = unlines $ [intercalate "," n, intersperse ',' t, s] ++ map show r




testRule :: String -> IO ()
testRule inp = case parse parseRule "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

getPLG :: String -> PLG
getPLG input = case parse parsePLG "" input of
            {   Left err -> error $ show err
            ;   Right ans -> ans
            }



getRule :: String -> Rule
getRule input = case parse parseRule "" input of
            {   Left err -> error "lul"
            ;   Right ans -> ans
            }


--parsePLG :: String -> Err PLG
--parsePLG = validate <=< left show . parse plgParser ""

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
parseRule = Rule <$> count 1 upper <* rightArrow <*> choice [string "#", rightSide] <*> string ""
--parseRule = Rule <$> count 1 upper <* rightArrow <*> ruleTerminalOrHash <*> ruleNonTerminal


-- parses non-empty right side of the rule
rightSide :: Parser String
rightSide = many1 lower <> choice [count 1 upper, string ""]

-- non-terminal symbols parsing
nonTerminalList :: Parser [String]
nonTerminalList = sepBy1 (count 1 upper) comma

-- terminal symbols parsing
terminalList :: Parser [Char]
terminalList = sepBy lower comma


ruleTerminalOrHash :: Parser [Char]
ruleTerminalOrHash = choice [string "#", many1 lower]

ruleNonTerminal :: Parser String
ruleNonTerminal = option "" (count 1 upper)

ruleTerminal :: Parser [Char]
ruleTerminal = many (satisfy (\char -> (char >= 'a' && char <= 'z') || char == '#'))

-- "->" symbol parser
rightArrow :: Parser String
rightArrow = string "->"

-- "," symbol parser
comma :: Parser Char
comma = char ','


validate :: PLG -> Err PLG
validate plg@PLG{..} = if allOK then Right plg else Left "invalid PLG"
  where
    allOK = True