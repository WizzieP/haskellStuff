data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Read, Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x == a = True
	| x < a = treeElem x left
	| x > a = treeElem x right

treeMax :: (Ord a) => Tree a -> Maybe a
treeMax EmptyTree = Nothing
treeMax (Node a _ EmptyTree) = Just a
treeMax (Node a _ right) = treeMax right

treeMin :: (Ord a) => Tree a -> Maybe a
treeMin EmptyTree = Nothing
treeMin (Node a EmptyTree _) = Just a
treeMin (Node a left _) = treeMin left

{-
treeDelete :: (Ord a) => a -> Tree a -> Tree a
treeDelete _ EmptyTree = EmptyTree
treeDelete _ (Node a EmptyTree EmptyTree) = EmptyTree
treeDelete x (Node a left right) 
	| x < a = Node a (treeDelete x left) right
	| x > a = Node a left (treeDelete x right)
	| x == a = 
-}



