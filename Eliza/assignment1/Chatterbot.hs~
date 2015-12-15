module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

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

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------


stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botBrain = do
  r <-randomIO :: IO Float
  let
    enhancedBrain = map (\y -> (fst y, (pick r (snd y)))) botBrain
    func question = rulesApply enhancedBrain question
    in return func

    
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = (maybe [] id .) . (transformationsApply "*" reflect)


reflect :: Phrase -> Phrase
reflect [] = []
reflect p = map (try (flip lookup reflections)) p


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
rulesCompile [] = []
rulesCompile ((a,b):xs) = (words(map toLower a) , map words b):rulesCompile(xs)





--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
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

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = try . (transformationsApply "*" id)

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute x y z = concatMap (\y -> if y == x then z else [y]) y


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just[]
match _ _ [] = Nothing
match _ [] _ = Nothing
match x (y:ys) (z:zs) 
 | y == z = (match x ys zs) 
 | y == x = orElse (singleWildcardMatch (y:ys) (z:zs)) (longerWildcardMatch (y:ys) (z:zs))
 | otherwise = Nothing




-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)




-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"
  
substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f list pattern = mmap ((substitute wc $ snd pattern).f) (match wc (fst pattern) list)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f p list = foldr1 (orElse) (map (transformationApply wc f list) p)
