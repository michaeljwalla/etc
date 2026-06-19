module Hangman ( format, GuessType (Correct, Incorrect, Elsewhere, OutOfBounds), pruneToAlpha, wordChecker, wordCheckerWithHints ) where
import General

-------------------------------- data types
data GuessType = Correct | Incorrect | Elsewhere | OutOfBounds | Impossible deriving (Show, Eq)
guessify "" _ = ""
guessify (c:cs) (g:gs)
    | g == Correct      = c:next
    | g == Incorrect    = '_':next
    | g == Elsewhere    = '!':next
    | g == OutOfBounds  = '?':next
    | otherwise         = '.':next --Impossible
    where next = guessify cs gs
-------------------------------- helpers

--helper (no bounds check)
slc = General.toLowerCase

fuzzeq :: String -> String -> Int -> Bool
fuzzeq as bs idx = (==) (slc (as !! idx)) (slc (bs !! idx))

fuzzidx :: Char -> String -> Int
fuzzidx c cs = indexOf (slc c) (map slc cs)

--drop an index (for hint functionality)
exclude :: Int -> String -> String
exclude _ "" = ""
exclude n (c:cs)
    | n == 0    = exclude (n-1) cs
    | otherwise = c:exclude (n-1) cs

--basic equality check
--correct word -> guessword -> guessword idx -> _?L
charChecker :: String -> String -> Int -> GuessType
charChecker correct guess idx
    | idx >= (length correct)           = OutOfBounds           --guess too long
    | not (isAlpha (curC))              = Correct               --punctuation/nonalpha shown
    | idx >= (length guess)             = Incorrect             --guess too short
    | fuzzeq correct guess idx          = Correct               --correct!
    | otherwise                         = Incorrect             --edge cases...?
    where
        curC = correct !! idx

--expects all 3 args same length
--its separate bc strings are shifting so Correct (==) would fail if not precomputed
reviseForHints :: String -> String -> [GuessType] -> [GuessType]
reviseForHints correct guess types = go pool guess types
    where
        pool = [c | (c, t) <- zip correct types, t /= Correct]
        go _ [] ts = ts
        go _ _ [] = []
        go available (g:gs) (t:ts)
            | t /= Incorrect = t : go available gs ts
            | otherwise      = t' : go available' gs ts
            where
                gIdx = fuzzidx g available
                available' = if gIdx /= -1 then exclude gIdx available else available
                t' = if gIdx /= -1 then Elsewhere else Incorrect


-------------------------------- exported funcs
format :: String -> [GuessType] -> String
format ans guesses = split ' ' (guessify ans guesses)
    where
        split c [] = []
        split c [x] = [x]
        split c (x:xs) = x:c:split c xs

--restrict input
pruneToAlpha :: String -> String
pruneToAlpha cs = [c | c <- cs, General.isAlpha c]


wordChecker :: String -> String -> [GuessType]
wordChecker correct guess = [charChecker correct guess (i-1) | i <- [1..larger]] where
    larger = max (length correct) (length guess)

wordCheckerWithHints :: String -> String -> [GuessType]
wordCheckerWithHints correct guess = reviseForHints correct guess (wordChecker correct guess)
