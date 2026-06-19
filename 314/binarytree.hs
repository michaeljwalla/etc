
data Person = Person Name Gender Age
    deriving (Show)
type Name = String
data Gender = M | F | U deriving (Show)
type Age = Int 

findPerson name (p@(Person n _ _):ps)
    | name == n = p
    | otherwise = findPerson name ps
--show (Gender)
--show (Person n g a) = n

data Tree a = Tnil | Node a (Tree a) (Tree a) deriving (Show)

findInOrder :: (a -> Bool) -> Tree a -> Maybe a
findInOrder _ Tnil = Nothing
findInOrder p (Node cur l r)
    | Just _ <- left    = left
    | p cur             = Just cur
    | Just _ <- right   = right
    | otherwise = Nothing
    where
        left = findInOrder p l
        right = findInOrder p r


--equivalent: buildBinTree [] = Tnil; buildBinTree xs = actualBuilder (reverse xs)
--since affix is depth-first, it runs as (affix n0 (affix n1 (affix n2 Tnil)))
--meaning the final elem is the root (inserted backwards)
--using foldl starts @ front, yielding
--(affix n0 (affix n1 (affix n2 Tnil))) (flipped for proper list iter) which is equiv to buildBinTree (reverse xs)
buildBinTree :: Ord a => [a] -> Tree a
buildBinTree = foldl (flip affix) Tnil
    where
        affix n Tnil = Node n Tnil Tnil
        affix n (Node x lx rx)
            | n < x     = Node x (affix n lx) rx
            | otherwise = Node x lx (affix n rx)

binTreeToList :: Tree a -> [a]
binTreeToList Tnil = []
binTreeToList (Node x lx rx) = (binTreeToList lx) ++ [x] ++ (binTreeToList rx)

treeFold :: out_t -> (a -> out_t -> out_t -> out_t) -> Tree a -> out_t
--same as fold (default) (operation) (input list)
treeFold term _ Tnil = term
treeFold term opn (Node x lx rx) = opn x (treeFold term opn lx) (treeFold term opn rx)

