-- FLP - Projekt 1
-- File: PLGParser.hs
-- Author: Peter Lukáč - xlukac11
-- March 2020
-- file containing data structures


{-# LANGUAGE RecordWildCards #-}

module PLG2NKA_Data where

import Data.List


-- rule consisting of left side and right side divided into terminals and non-terminals
data Rule = Rule
    { leftSide :: String
    , rightSideTerminals :: [Char]
    , rightSideNonTerminal :: String
    } deriving (Eq)
instance Show Rule where
    show (Rule l r1 r2) = l ++ "->" ++ r1 ++ r2


-- grammar structure
data PLG = PLG
    { nonTerminals :: [String]
    , terminals :: [Char]
    , start :: String
    , rules :: [Rule]
    } deriving (Eq)
instance Show PLG where
    show (PLG n t s r) = unlines $ [intercalate "," n, intersperse ',' t, s] ++ map show r


-- transition structure of the NKA
data Transition = Transition
    { fromState :: Int
    , byInput :: Char
    , toState :: Int
    } deriving (Eq)
instance Show Transition where
    show (Transition f b t) = (show f) ++ "," ++ [b] ++ "," ++ (show t)


-- finite automata structure
data NKA = NKA
    { states :: [Int]
    , startState :: Int
    , endStates :: [Int]
    , transitions :: [Transition]
    } deriving (Eq)
instance Show NKA where
    show (NKA s s0 e t) = unlines $ [intercalate "," (map show s), (show s0), 
                                    intercalate "," (map show e)] ++ map show t