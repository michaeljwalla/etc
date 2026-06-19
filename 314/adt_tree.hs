module Tree ( Tree, toLeaf, toBranch, valueOf, leftBranch, rightBranch, isLeaf ) where
data Tree a -- just the type name
leaf :: a -> Tree a -- construct a leaf
branch :: a -> Tree a -> Tree a -> Tree a -- construct a branch
cell :: Tree a -> a -- return a value of the tree
left, right :: Tree a -> Tree a -- return left or right subtree
isLeaf :: Tree a -> Bool -- check is a leaf


