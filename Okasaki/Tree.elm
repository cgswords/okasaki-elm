module Okasaki.Tree where

type Tree a 
     = Empty
     | Node (Tree a) a (Tree a)

empty : Tree a
empty = Empty

insert : (a -> a -> Order) -> a -> Tree a -> Tree a
insert f x t = 
  case t of
    Empty      -> Node Empty x Empty
    Node l a r -> case f x a of
                   LT -> Node (insert f x l) a r 
                   EQ -> t
                   GT -> Node l a (insert f x r)

member : (a -> a -> Order) -> a -> Tree a -> Bool
member f x t = 
  case t of
    Empty      -> False 
    Node l a r -> case f x a of
                    LT -> member f x l
                    EQ -> True
                    GT -> member f x r
