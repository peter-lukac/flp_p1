--merge [] l2 = l2
--merge l1 [] = l1
--merge l1@(x:xs) l2@(y:ys) =
--    if x<y
--        then x:merge xs l2
--        else y:merge l1 ys

--sumlist [] = 0
--sumlist [a] = a
--sumlist (x:xs) = x + sumlist xs

{-# LANGUAGE RecordWildCards #-}

import Data.List

data Rule = Rule
    { leftSide :: String
    , rightSideTerminals :: [Char]
    , rightSideNonTerminal :: String
    } deriving (Eq)
instance Show Rule where
    show (Rule l r1 r2) = l ++ " -> " ++ r1 ++ "+" ++ r2


data PLG = PLG
    { nonTerminals :: [String]
    , terminals :: [Char]
    , start :: String
    , rules :: [Rule]
    } deriving (Eq)
instance Show PLG where
    show (PLG n t s r) = unlines $ [intercalate "," n, intersperse ',' t, s] ++ map show r

data RuleAndNonTs = RuleAndNonTs [Rule] [String]


splitRule :: Rule -> [String] -> [Rule]
-- when rule is A->xB then return it
splitRule (Rule a (b:[]) c) nonTs = [Rule a [b] c]
-- when rule is A->x..xB then split it
splitRule (Rule a (b:bs) c) nonTs = (Rule a [b] newNonT):splitRule (Rule newNonT bs c) (newNonT:nonTs)
    where 
        newNonT = (head a):(show $ length $ filter (\x -> x == (head a)) (map head nonTs))


transformRules :: [Rule] -> [Rule]
-- splitting last rule
transformRules (rule:[]) = splitRule rule [""]
-- splitting all rules
transformRules (rule:rules) = do
    -- split rule with and get new non terminals
    let x = splitRule rule ["A", "B", "C"]
    let y = transformRules rules
    concat [x, y]


testP :: Rule -> [String] -> RuleAndNonTs
-- 
testP (Rule a (b:[]) c) nonTs = RuleAndNonTs [Rule a [b] c] nonTs
-- 
testP (Rule a (b:bs) c) nonTs = RuleAndNonTs ((Rule a [b] newNonT):newRules) newNonTs
    where
        newNonT = (head a):(show $ length $ filter (\x -> x == (head a)) (map head nonTs))
        RuleAndNonTs newRules newNonTs = testP (Rule newNonT bs c) (newNonT:nonTs)


testP2 :: [Rule] -> [String] -> RuleAndNonTs
--
testP2 (rule:[]) nonTs = testP rule nonTs
--
testP2 (rule:rules) nonTs = RuleAndNonTs (concat [rules1, rules2]) nonTs2
    where
        RuleAndNonTs rules1 nonTs1 = testP rule nonTs
        RuleAndNonTs rules2 nonTs2 = testP2 rules nonTs1


playP :: IO ()
playP = do
    let n = ["A", "B", "C"]
    let rule1 = Rule "A" "a" "B"
    let rule2 = Rule "B" "bcd" "C"
    let rule3 = Rule "B" "ef" ""
    let rule4 = Rule "B" "#" ""
    let rules = [rule1, rule2, rule3, rule4]
    let RuleAndNonTs a b = testP2 rules n
    --let RuleAndNonTs a b = testP rule3 n
    print b
    print ""
    putStr $ unlines $ map show a


play :: IO ()
play = do
    let rule1 = Rule "A" "a" "B"
    let rule2 = Rule "B" "bcd" "C"
    let rule3 = Rule "B" "ef" ""
    let rule4 = Rule "B" "#" ""
    let rules1 = [rule1, rule2, rule3, rule4]
    putStr $ unlines $ map show rules1
    let rules2 = transformRules rules1
    print ""
    putStr $ unlines $ map show rules2


{-
adjustRule :: Rule -> Rule
adjustRule rule@Rule{..} = Rule leftSide rightSide1 rightSide2
    where
        rightSide1 = filter (\x -> (x >= 'a' && x <= 'z') || x == '#') rightSideTerminals
        rightSide2 = filter (\x -> x >= 'A' && x <= 'Z') rightSideTerminals

play :: IO ()
play = do
    let rule1 = Rule "A" "aB" ""
    let rule2 = Rule "A" "amB" ""
    let rule3 = Rule "A" "ax" ""
    let rule4 = Rule "A" "x" ""
    let rule5 = Rule "A" "#" ""
    let rules1 = [rule1, rule2, rule3, rule4, rule5]
    let rules2 = map adjustRule rules1
    print $ map show rules1
    print $ map show rules2
    putStrLn ""
    print $ show rule1
    print $ show rule2
    print $ show rule3
    print $ show rule4
    print $ show rule5
    putStrLn ""
    print $ show $ adjustRule rule1
    print $ show $ adjustRule rule2
    print $ show $ adjustRule rule3
    print $ show $ adjustRule rule4
    print $ show $ adjustRule rule5
-}
    
