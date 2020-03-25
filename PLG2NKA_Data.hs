
{-# LANGUAGE RecordWildCards #-}

module PLG2NKA_Data where

import Data.List

data Rule = Rule
    { leftSide :: String
    , rightSideTerminals :: [Char]
    , rightSideNonTerminal :: String
    } deriving (Eq)
instance Show Rule where
    --show (Rule l r1 r2) = l ++ "->" ++ case r1 ++ r2 of { "" -> "#"; _ -> r1 ++ r2}
    show (Rule l r1 r2) = l ++ "->" ++ r1 ++ r2

data PLG = PLG
    { nonTerminals :: [String]
    , terminals :: [Char]
    , start :: String
    , rules :: [Rule]
    } deriving (Eq)
instance Show PLG where
    show (PLG n t s r) = unlines $ [intercalate "," n, intersperse ',' t, s] ++ map show r


data Transition = Transition
    { fromState :: Int
    , byInput :: Char
    , toState :: Int
    } deriving (Eq)
instance Show Transition where
    show (Transition f b t) = (show f) ++ "," ++ [b] ++ "," ++ (show t)

data NKA = NKA
    { states :: [Int]
    , startState :: Int
    , endStates :: [Int]
    , transitions :: [Transition]
    } deriving (Eq)
instance Show NKA where
    show (NKA s s0 e t) = unlines $ [intercalate "," (map show s), (show s0), intercalate "," (map show e)] ++ map show t