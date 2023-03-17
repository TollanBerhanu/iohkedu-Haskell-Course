{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module HigherOrder where

import Data.List (foldl', sortBy, sort)
import Prelude hiding (take, product, reverse, all)

-- These are binary trees with labels in their nodes.

data BinTree a =
    Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

-- Task HigherOrder-1.
--
-- Define 'product' both using an accumulator explicitly,
-- and using (strict) foldl'.

-- |
-- >>> product [1 .. 4]
-- 24
--
product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * (product xs)

-- |
-- >>> product [3, 11]
-- 33
--
product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- Task HigherOrder-2.
--
-- Define 'reverse' using 'foldl'.

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x:acc) []

-- Task HigherOrder-3.
--
-- Define a Functor instance for binary trees. For
-- this, we have to define a map function on binary
-- trees and then define the class instance.
--
-- The instance is actually given below. You just
-- have to uncomment it.

-- |
-- >>> mapBinTree (+1) (Bin Empty 7 (Bin Empty 8 Empty))
-- Bin Empty 8 (Bin Empty 9 Empty)
--
mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Bin binLeft node binRight) = Bin (mapBinTree f binLeft) (f node) (mapBinTree f binRight) 


instance Functor BinTree where
  fmap = mapBinTree


-- Task HigherOrder-4.
--
-- The 'BinTree' type is suitable for representing
-- "binary search trees".
--
-- Binary search trees are trees that store their elements
-- in order, so that we can efficiently find elements by comparing
-- the element we are looking for with the current node, and
-- descending either left or right.
--
-- Define a function 'isBST' that checks if a given 'BinTree'
-- is a binary search tree.

isBST :: Ord a => BinTree a -> Bool
isBST binTree = case binTree of
                Empty -> True
                (Bin Empty _ Empty) -> True
                (Bin Empty node binRight) -> (node < getNode binRight)
                (Bin binLeft node Empty) -> (node > getNode binLeft)
                (Bin binLeft node binRight) -> (node > getNode binLeft) && (node < getNode binRight)
    where getNode (Bin _ n _) = n

-- Task HigherOrder-5.
--
-- Define a function 'search' that looks up a value in a BST.
--
-- From now on, we use a type synonym to signal that a certain
-- binary tree should in fact be a binary search tree, even if
-- the type system does not actively enforce this.

type BST a = BinTree a

search :: Ord a => a -> BST a -> Bool
search _ Empty = False 
search item (Bin bstLeft node bstRight)
  | item > node = search item bstRight
  | item < node = search item bstLeft
  | otherwise = True

-- Task HigherOrder-6.
--
-- Define a function 'insert' that inserts a value into a BST
-- while maintaining the BST property. (Don't worry about balancing
-- the tree. That's not important for now. But do make sure you
-- maintain the BST property itself.)

insert :: Ord a => a -> BST a -> BST a
insert item Empty = Bin Empty item Empty
insert item (Bin binLeft node binRight)
  | node > item = Bin (insert item binLeft) node binRight
  | node < item = Bin binLeft node (insert item binRight)
  | otherwise = Bin binLeft item binRight

-- Task HigherOrder-7.
--
-- Define the function 'all' (as in the Prelude) using 'foldr'.
-- Hide the original binding from the Prelude by exluding it in
-- the module header. Provide the type signature yourself.

-- |
-- >>> all even [2, 4 .. 20]
-- True
--
-- >>> all odd [1, 1, 1, 2, 3, 3]
-- False
--
-- TODO: define all
all :: (Eq a) => (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) = f x && all f xs

-- Task HigherOrder-8.
--
-- Import the function 'sortBy' from the 'Data.List' module.
-- Then use this function to define a function that sorts a
-- list in descending rather than ascending order.

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy (\x y -> y `compare` x)

-- Task HigherOrder-9.
--
-- Use 'insert' and 'foldr' to create a BST from a list.

fromListBST :: Ord a => [a] -> BST a
fromListBST = foldr (insert) Empty

-- Task HigherOrder-10.
--
-- We want to attach unique numbers to each node in a binary
-- tree, so that all the numbers from left to right are labelled
-- in ascending order.
--
-- NOTE: This is not easy. Think about this and discuss your
-- strategy with us before you proceed.

-- |
-- >>> labelTree $ Bin Empty 'x' (Bin Empty 'y' Empty)
-- Bin Empty ('x',1) (Bin Empty ('y',2) Empty)
--
-- >>> labelTree $ Bin (Bin Empty 1 Empty) 2 (Bin Empty 5 Empty)
-- Bin (Bin Empty (1,1) Empty) (2,2) (Bin Empty (5,3) Empty)
--
treeToList :: BinTree a -> [a]
treeToList Empty = []
treeToList (Bin left node right) = node:(treeToList left ++ treeToList right)

labelTree :: (Eq a, Ord a) => BinTree a -> BinTree (a, Int)
labelTree binTree = fmap addLabels binTree
  where mappings = zip (sort $ treeToList binTree) [1..]
        addLabels = (\n -> head [(x,lbl) | (x,lbl) <- mappings, x == n])

-- Task HigherOrder-11.
--
-- Another form of tree labeling does not use an integer, but
-- a label supply that is given as a list. So write a variant
-- of 'labelTree' that takes the labels from a list, but uses
-- every label only once. You may assume in this function that
-- the list contains infinitely many (or at least sufficiently
-- many) labels, so you don't have to return a 'Maybe' if the
-- list is too short, but can just crash.

-- |
-- >>> labelTree' (Bin Empty 1 (Bin Empty 42 Empty)) "Haskell"
-- Bin Empty (1,'H') (Bin Empty (42,'a') Empty)
--
labelTree' :: (Eq a, Ord a) => BinTree a -> [b] -> BinTree (a, b)
labelTree' binTree labels = fmap addIndex binTree
  where mappings = zip (sort $ treeToList binTree) labels
        addIndex = (\n -> head [(x,lbl) | (x,lbl) <- mappings, x == n])

-- Task HigherOrder-12.
--
-- Define the catamorphism on 'BinTree'.
-- Also come up with the type signature yourself.
-- Look at functions such as 'mapBinTree' and 'search'
-- above for inspiration. Also try to
-- rewrite these in terms of the catamorphism once you
-- are done.

-- Task HigherOrder-13.
--
-- Try to implement the function 'take' on lists using 'foldr'.
--
-- Once again, this is not easy, and you should discuss your
-- ideas with us before trying.
--
-- Consider the type signature and a possible definition of
-- take, and note that not just the list is being traversed,
-- but also the number changes.

-- |
-- >>> take 3 "Haskell"
-- "Has"
--
take :: Int -> [a] -> [a]
take _ []       = []
take n (x : xs)
  | n > 0     = x : take (n - 1) xs
  | otherwise = []

-- take' :: Int -> [a] -> [a]
-- take' n xs = foldr (\x acc -> x:acc) [] xs --[n..(length xs)]

-- Task HigherOrder-14.
--
-- If you succeeded in defining 'take' in terms of 'foldr',
-- then perhaps it will not surprise you all that much that
-- even 'foldl' can be written in terms of 'foldr'.
--
-- Try to do this. The approach required is similar.

-- |
-- >>> myFoldl (\xs x -> xs ++ [x]) [] "Haskell"
-- "Haskell"
--
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl = error "TODO: implement myFoldl"

-- Task HigherOrder-15.
--
-- Define a Foldable instance for binary trees.
-- For this, there are several methods, but the
-- easiest with our knowledge so far is to implement
-- a foldr function on trees.
-- For this, one option might be to first convert a
-- binary tree to a list.

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree f initial binTree = foldr f initial toList
  where toList = treeToList binTree


instance Foldable BinTree where
  foldr = foldrBinTree

