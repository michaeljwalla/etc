quicksort [] = []
quicksort (x:xs) = quicksort le ++ [x] ++ quicksort ge
    where
        le = [n | n <- xs, n <= x]
        ge = [n | n <- xs, n >= x]

sequenceActions :: [IO a] -> IO [a]
sequenceActions [] = return []
sequenceActions (axn:axns) = do
    x <- axn                     --perform action, assign to x
    xs <- sequenceActions axns  --recursively fill array
    return (x:xs)                --recombine array and return
