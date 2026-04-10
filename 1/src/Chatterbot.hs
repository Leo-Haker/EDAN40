{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)
import Data.Maybe (Maybe(Nothing))
import qualified Data.Maybe as Data

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]



chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()


--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
{- TO BE WRITTEN -}
makePair = undefined

funkarDetta1 = (mkPattern "*" (Prelude.words"My name is *"), mkPattern "*" (Prelude.words "Je m'appelle *"))
funkarDetta2 = (mkPattern "+" (Prelude.words"My name a *"), mkPattern "+" (Prelude.words "Je m'buuu *"))
medDetta1 = ["My", "name", "is", "Zacharias"]

funkis =[funkarDetta2, funkarDetta1]

-- >>>  rulesApply funkis medDetta1
-- ["Je","m'appelle","Zacharias"]

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN 
1. Match the phrase with a pattern.
2. Reflect the match.
3. Substitute the match in the target pattern.
-}
rulesApply pts phrase = fromMaybe [] (transformationsApply reflect pts phrase)


-- >>>reflect ["i", "will", "never", "see", "my","reflection", "in", "your", "eyes"] 

{-
Facit 
["you", "will", "never", "see", "your",
         "reflection", "in", "my", "eyes"]
-}
        
reflect :: Phrase -> Phrase
{- TO BE WRITTEN -} 
reflect ps = map(\p -> foldr(\r acc -> if p == (fst r) then (snd r) else acc)p reflections) ps
reflections =
    [ ("am",     "are"),
        ("was",    "were"),
        ("i",      "you"),
        ("i'm",    "you are"),
        ("i'd",    "you would"),
        ("i've",   "you have"),
        ("i'll",   "you will"),
        ("my",     "your"),
        ("me",     "you"),
        ("are",    "am"),
        ("you're", "i am"),
        ("you've", "i have"),
        ("you'll", "i will"),
        ("your",   "my"),
        ("yours",  "mine"),
        ("you",    "me")
    ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
{- TO BE WRITTEN -}
ruleCompile (p, ts) = Rule(starPattern $ map toLower p, map(starPattern . map toLower)ts) -- samma som map(\t starPattern $ map toLower t)ts)

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- >>> mkPattern '*' "Hi *!"
-- Pattern [Item 'H',Item 'i',Item ' ',Wildcard,Item '!']



mkPattern :: Eq a => a -> [a] -> Pattern a
{- TO BE WRITTEN -}
mkPattern a xs = Pattern (map(\x -> if x == a then Wildcard else Item x)xs) 

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply = undefined


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument
substitute :: Eq a => Template a -> [a] -> [a]
{- TO BE WRITTEN -}
substitute (Pattern t) s =  concatMap(\x -> case x of 
                                Wildcard -> s 
                                Item x-> [x])t


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.




-- >>>match (mkPattern '*' "frodo") "gandalf" 
-- >>>match (mkPattern 'x' "2*x+3+x") "2*7+3" 
-- >>>match (mkPattern 'x' "abcd") "abcd" 
-- >>>match (mkPattern 2 [1,3..5]) [1,3..5]
-- >>>match (mkPattern 'x' "2*x+3") "2*7+3"
-- >>>match (mkPattern '*' "* and *") "you and me"
-- Nothing
-- Nothing
-- Just ""
-- Just []
-- Just "7"
-- Just "you"


{-
FACIT:
Nothing
Nothing
Just[]
Just[]
Just["7"]
Just["you"]

-}
p1 :: Pattern Char
p1 = mkPattern '*' "hej"
p2 :: Pattern Char
p2 = mkPattern '*' "h*ej"

s1 :: [Char] 
s1 = "hej"
s2 :: [Char] 
s2 = "bheej"
-- >>> match p1 s1
-- >>> match p2 s2
-- Just ""
-- Nothing
a = mkPattern '*' "* and *"
-- >>>match a "you and me"
-- Just "you"
match :: Eq a => Pattern a -> [a] -> Maybe [a]
match (Pattern []) [] = Just []  -- Vi saknade detta base case
match (Pattern _) [] = Nothing
match (Pattern []) _ = Nothing
match pattern@(Pattern (p:ps)) list@(x:xs) 
      | p == Wildcard = orElse (singleWildcardMatch pattern list) (longerWildcardMatch pattern list)
      | p == Item x = match (Pattern ps) xs -- kör match efter tagit bort nästa element
      | otherwise = Nothing -- de två tidigare, om listorna inte är samma

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) = mmap (const [x]) (match (Pattern ps) xs)
longerWildcardMatch _ [] = Nothing
longerWildcardMatch pattern@(Pattern (Wildcard:ps)) (x:xs) = mmap (x:) (match pattern xs)

{- TO BE WRITTEN -}
{- OVAN ÄR EKVIVALENT MED TIDIGARE SKRIVNA NEDAN:

singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _ -> Just [x]

longerWildcardMatch (Pattern (Wildcard:ps)) xs = 
    case xs of 
        [] -> Nothing 
        (x:xs') -> case match (Pattern (Wildcard:ps)) xs' of
                    Nothing -> Nothing
                    Just list -> Just(x:list)
-}





-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat) -- f . g = f (g x). Alltså betyder detta mmap tranform (match pat xs)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply transform xs (ps, ts)=  mmap (substitute ts) (matchAndTransform transform ps xs)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply _ [] _ = Nothing
transformationsApply transform (pt: pts) xs = orElse success tryAgain
  where
    success = transformationApply transform xs pt
    tryAgain = transformationsApply transform pts xs


-- TESTER --

frenchPresentation :: (Pattern Char, Template Char)
frenchPresentation = (mkPattern '*' "My name is *", mkPattern '*' "Je m'appelle *")
frenchPresentation1 = (mkPattern '+' "My name is *", mkPattern '+' "Kalle anka *")
frenchPresentation2 = (mkPattern '*' "My name is +", mkPattern '*' "fel fel +")
frenchPresentation3 = (mkPattern '+' "My name is +", mkPattern '+' "Jag heter +")

funkarDetta = (mkPattern "*" (Prelude.words"My name is *"), mkPattern "*" (Prelude.words "Je m'appelle *"))
ab =  (mkPattern '*' "My name is *", mkPattern '*' "Je m'appelle *")
medDetta = ["My", "name", "is", "Zacharias"]

test1 = [frenchPresentation1, frenchPresentation2]

test2 :: [(Pattern Char, Pattern Char)]
test2 = [frenchPresentation1, frenchPresentation2, frenchPresentation]
test3 = [frenchPresentation1, frenchPresentation2, frenchPresentation3]

-- >>> transformationApply id "My name is Zacharias" frenchPresentation
-- Just "Je m'appelle Zacharias"
-- >>>  Prelude.unwords $ Data.fromMaybe [] (transformationApply id medDetta funkarDetta)
-- "Je m'appelle Zacharias"

-- >>> ab
-- >>> funkarDetta
-- (Pattern [Item 'M',Item 'y',Item ' ',Item 'n',Item 'a',Item 'm',Item 'e',Item ' ',Item 'i',Item 's',Item ' ',Wildcard],Pattern [Item 'J',Item 'e',Item ' ',Item 'm',Item '\'',Item 'a',Item 'p',Item 'p',Item 'e',Item 'l',Item 'l',Item 'e',Item ' ',Wildcard])
-- (Pattern [Item "My name is *"],Pattern [Item "Je m'appelle *"])



-- >>> transformationsApply id test1 "My name is Zacharias"
-- >>> transformationsApply id test2 "My name is Zacharias"
-- >>> transformationsApply id test3 "My name is Zacharias"
-- Nothing
-- Just "Je m'appelle Zacharias"
-- Just "Jag heter Zacharias"
