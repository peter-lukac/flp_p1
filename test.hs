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
import System.Exit (die)

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


play = do
    let ls = ["M", "N", "C", "M"]
    print $ all (\x -> length x == 1 ) (map (`elemIndices` ls) ls)


{-
validate :: PLG -> Either String PLG
validate plg@PLG{..} = if ok then Right plg else Left "invalid PLG"
    where
        ok = elem start nonTerminals
            && all ((`elem` nonTerminals) . leftSide) rules
            && all ((all (`elem` '#':terminals)) . rightSideTerminals) rules
            && all ((`elem` "":nonTerminals) . rightSideNonTerminal) rules

play :: IO ()
play = do
    let plg = PLG ["A", "B"] ['a', 'b'] "A" [Rule "A" "ab" "B", Rule "B" "b" "", Rule "B" "#" ""]
    --let x = case validate plg of { Left err -> error err; Right y -> y }
    case validate plg of
        Left e -> do
            error e
        _ -> do
            return ()
    putStr $ show plg
-}

{-
splitRule :: Rule -> [String] -> RuleAndNonTs
-- when rule is A->xB then return it
splitRule (Rule a (b:[]) c) nonTs = RuleAndNonTs [Rule a [b] c] nonTs
-- when rule is A->x..xB then split it
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


play :: IO ()
play = do
    let n = ["A", "B", "C"]
    let rule1 = Rule "A" "a" "B"
    let rule2 = Rule "B" "bcd" "C"
    let rule3 = Rule "B" "ef" ""
    let rule4 = Rule "B" "#" ""
    let rules = [rule1, rule2, rule3, rule4]
    let RuleAndNonTs a b = transformRules rules n
    --let RuleAndNonTs a b = testP rule3 n
    print b
    print ""
    putStr $ unlines $ map show a
-}

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
    
