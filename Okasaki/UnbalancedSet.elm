module Okasaki.UnbalancedSet where

import Okasaki.Tree

type alias UnbalancedSet a =
           { value : Okasaki.Tree.Tree a
           , comparator : a -> a -> Order
            }

empty : (a -> a -> Order) -> UnbalancedSet a
empty c = {value = Okasaki.Tree.Empty, comparator = c}

insert : a -> UnbalancedSet a -> UnbalancedSet a
insert x t = {t | value <- Okasaki.Tree.insert t.comparator x t.value}

member : a -> UnbalancedSet a -> Bool
member x t = Okasaki.Tree.member t.comparator x t.value

