module Okasaki.Stack where

import List exposing ((::))

type alias Stack a = List a

empty : Stack a
empty = []

isEmpty : Stack a -> Bool
isEmpty s = List.isEmpty s

push : a -> Stack a -> Stack a
push x s = x :: s

peek : Stack a -> Maybe a
peek s =
  case s of
    []        -> Nothing
    (x :: xs) -> Just x

pop : Stack a -> Stack a
pop s =
  case s of
    []      -> []
    (x::xs) -> xs

{-| toList : Stack a -> Stack a
Returns the stack, top to bottom, as a list.
|-}
toList : Stack a -> List a
toList s = s
