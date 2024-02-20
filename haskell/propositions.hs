{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- propositions.hs
{-
Haskell Propositions
Developer Name : Neil Mathias     
Developer E-mail : neilmathias25@gmail.com
-}

--import Tautology  -- Do not import anything else!

data Prop =  Const Bool
          | Var Char
          | Not Prop
          | Or Prop Prop
          | And Prop Prop
          | Imply Prop Prop
--
-- Part 1: Some Additions to tautology.hs
--

-- Question 1: Showing Propositions

instance Show Prop where
    show (Const True)  = "true"
    show (Const False)  = "false"
    show (Var p) = [p]
    show (Not a) = "(not " ++ show a ++ ")"
    show (And a b) = "(" ++ show a ++ " and " ++ show b ++ ")"
    show (Imply a b) = "(" ++ show a ++ " --> " ++ show b ++ ")"

-- Question 2: Comparing Propositions

instance Eq Prop where
    (==) (Const a) (Const b) = a == b
    (==) (Const a) (Var b) = False 
    (==) (Const a) (Not b) = False 
    (==) (Const a) (And b c) = False
    (==) (Const a) (Imply b c) = False 
    (==) (Var a) (Const b) = False
    (==) (Var a) (Var b) = a == b
    (==) (Var a) (Not b) = False 
    (==) (Var a) (And b c) = False 
    (==) (Var a) (Imply b c) = False 
    (==) (Not a) (Const b) = False
    (==) (Not a) (Var b) = False
    (==) (Not a) (Not b) = a == b
    (==) (Not a) (And b c) = False 
    (==) (Not a) (Imply b c) = False  
    (==) (And a b) (Const c) = False 
    (==) (And a b) (Var c) = False 
    (==) (And a b) (Not c) = False
    (==) (And a b) (And c d) = a == c && b == d 
    (==) (And a b) (Imply c d) = False
    (==) (Imply a b) (Const c) = False 
    (==) (Imply a b) (Var c) = False
    (==) (Imply a b) (Not c) = False
    (==) (Imply a b) (And c d) = False
    (==) (Imply a b) (Imply c d) = a == c && b == d
    (==) (Imply (And a b) c) (Imply (And d e) f) = False

--
-- Part 2: nand-only Propositions
--

data NandOnly = Nconst Bool
              | Nvar Char
              | Nand NandOnly NandOnly

-- Question 3: Showing nand-only Propositions

instance Show NandOnly where
    show (Nconst True)  = "true"
    show (Nconst False)  = "false"
    show (Nvar p) = [p]
    show (Nand a b) = "(" ++ show a ++ " | " ++ show b ++ ")"

-- Question 4: Comparing nand-only Propositions

instance Eq NandOnly where
    (==) (Nconst a) (Nconst b) = a == b
    (==) (Nvar a) (Nvar b) = a == b
    (==) (Nand a b) (Nand c d) = a == c && b == d

-- Question 5: Counting nands

count :: Num p => String -> p
count "" = 0
count str = if last str == '|' 
                then (1 + count (init str)) 
                else count (init str)

nandCount :: Num p => NandOnly -> p
nandCount nc = count (show (nc))

-- Question 6: Translating Prop Expressions to NandOnly Expressions

--indent show(prop) for variable extraction
formatProp :: String -> String
formatProp "" = ""
formatProp str | head str == '(' = take 1 str ++ " " ++ formatProp (tail str)
               | head str == ')' = " " ++ take 1 str ++ formatProp (tail str)
               | otherwise = take 1 str ++ formatProp(tail str)

getVar :: [String] -> String
getVar [] = ""
getVar str | head str == "(" || head str == ")" || head str == "not" || head str == "and" || head str == "-->" || head str == "true" || head str == "false"= getVar (tail str)
           | otherwise = (head str) ++ getVar (tail str)


toNand :: Prop -> NandOnly
toNand prop | prop == (Not (Const True)) = (Nand (Nconst True) (Nconst True))
            | prop == (Not (Const False)) = (Nand (Nconst False) (Nconst False))
            | prop == (Not (Var fvar)) = (Nand (Nvar fvar) (Nvar fvar)) 
            | prop == (Not (And (Var fvar) (Var svar))) = (Nand (toNand (And (Var fvar) (Var svar))) (toNand (And (Var fvar) (Var svar)))) 
            | prop == (Not (Imply (Var fvar) (Var svar))) = (Nand (toNand (Imply (Var fvar) (Var svar))) (toNand (Imply (Var fvar) (Var svar))))
            | prop == (Not (And (Const True) (Var fvar))) = (Nand (toNand (And (Const True) (Var fvar))) (toNand (And (Const True) (Var fvar)))) 
            | prop == (Not (And (Const False) (Var fvar))) = (Nand (toNand (And (Const False) (Var fvar))) (toNand (And (Const False) (Var fvar)))) 
            | prop == (Not (And (Const True) (Imply (Var fvar) (Var svar)))) = (Nand (toNand (And (Const True) (Imply (Var fvar) (Var svar)))) (toNand (And (Const True) (Imply (Var fvar) (Var svar)))))
            | prop == (Not (And (Const False) (Imply (Var fvar) (Var svar)))) = (Nand (toNand (And (Const False) (Imply (Var fvar) (Var svar)))) (toNand (And (Const False) (Imply (Var fvar) (Var svar)))))
            --and                 not|^ 
            | prop == (And (Const True) (Var fvar)) = (Nand (Nand (Nconst True) (Nvar fvar)) (Nand (Nconst True) (Nvar fvar)))
            | prop == (And (Const False) (Var fvar)) = (Nand (Nand (Nconst False) (Nvar fvar)) (Nand (Nconst False) (Nvar fvar)))
            | prop == (And (Var fvar) (Var svar)) = (Nand (Nand (Nvar fvar) (Nvar svar)) (Nand (Nvar fvar) (Nvar svar)))
            | prop == (And (Not (Var fvar)) (Var svar)) = (Nand (Nand (toNand (Not (Var fvar))) (Nvar svar)) (Nand (toNand (Not (Var fvar))) (Nvar svar)))
            | prop == (And (Var fvar) (Not (Var svar))) = (Nand (Nand (Nvar fvar) (toNand (Not (Var svar)))) (Nand (Nvar fvar) (toNand (Not (Var svar)))))
            | prop == (And (Const True) (Imply (Var fvar) (Var svar))) = (Nand (Nand (Nconst True) (toNand (Imply (Var fvar) (Var svar)))) (Nand (Nconst True) (toNand (Imply (Var fvar) (Var svar))))) 
            | prop == (And (Const False) (Imply (Var fvar) (Var svar))) = (Nand (Nand (Nconst False) (toNand (Imply (Var fvar) (Var svar)))) (Nand (Nconst False) (toNand (Imply (Var fvar) (Var svar))))) 
            | prop == (And (Var fvar) (Imply (Var svar) (Var tvar))) = (Nand (Nand (Nvar fvar) (toNand (Imply (Var svar) (Var tvar)))) (Nand (Nvar fvar) (toNand (Imply (Var svar) (Var tvar)))))
            --imply               and|^
            | prop == (Imply (Var fvar) (Var svar)) = (Nand (Nvar fvar) (Nand (Nvar svar) (Nvar svar)))
            | prop == (Imply (And (Var fvar) (Var svar)) (Var tvar)) = (Nand (toNand (And (Var fvar) (Var svar))) (Nand (Nvar tvar) (Nvar tvar)))
            | prop == (Imply (Var fvar) (And (Var svar) (Var tvar))) = (Nand (Nvar fvar) (Nand (toNand (And (Var svar) (Var tvar))) (toNand (And (Var svar) (Var tvar)))))
            | prop == (Imply (And (Var fvar) (Imply (Var svar) (Var tvar))) (Var fovar)) = (Nand (toNand (And (Var fvar) (Imply (Var svar) (Var tvar)))) (Nand (Nvar fovar) (Nvar fovar)))
            where 
                fp = formatProp (show(prop))        
                w = words fp
                variables = getVar w
                fvar = (head variables)
                svar = (head (tail variables))
                tvar = (head (tail (tail variables)))
                fovar = (head (tail (tail (tail variables))))


-- Question 7: Evaluating nand-only Propositions
-- Need to solve 'find'
--evalNand :: [(Char,Bool)] -> NandOnly -> Bool
--evalNand _ (Nconst b) = b 
--evalNand sub (Nvar x) = find x sub
--evalNand sub (Nand a b) = not (evalNand sub a && evalNand sub b) 

-- Question 8: Evaluating nand-only Propositions Using Maybe

contains :: Char -> [(Char,Bool)] -> Bool
contains var [] = False
contains var sub = if (fst (head sub)) == var
                       then True 
                       else contains var (tail sub)

mnot :: Maybe Bool -> Maybe Bool
mnot (Just b) = Just b
mnot Nothing  = Nothing

-- Need to solve Subst
--evalNand2 :: Subst -> NandOnly -> Maybe Bool
--evalNand2 _ (Nconst b) = Just b
--evalNand2 sub (Nvar x) = if (contains x sub) == True 
--                             then mnot (Just (find x sub)) 
--                             else mnot Nothing 
--evalNand2 sub (Nand a b) | (evalNand2 sub a) == Just True && (evalNand2 sub b) == Just True = mnot (Just False)
--                         | (evalNand2 sub a) == Nothing || (evalNand2 sub b) == Nothing = mnot Nothing
--                         | otherwise = mnot (Just True)