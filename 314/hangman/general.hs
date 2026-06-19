module General ( rep, indexOf, isAlpha, toUpperCase, toLowerCase ) where

rep n xs = foldl (++) [] [xs | _ <- [1..n]]

indexOf :: Eq a => a -> [a] -> Int
indexOf n xs  = countLookup n xs 0
    where
        countLookup _ [] _      = -1
        countLookup n (x:xs) i
            | n == x    = i
            | otherwise = countLookup n xs (i+1)

isAlpha :: Char -> Bool
isAlpha = ((-1 /=) . ((flip indexOf) (['a'..'z']++['A'..'Z'])))

toUpperCase :: Char -> Char
toUpperCase c
    | i /= -1       = ['A'..'Z'] !! i
    | otherwise     = c
    where
        i = indexOf c ['a'..'z']

toLowerCase :: Char -> Char
toLowerCase c
    | i /= -1       = ['a'..'z'] !! i
    | otherwise     = c
    where
        i = indexOf c ['A'..'Z']
