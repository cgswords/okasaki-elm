module Okasaki.List where

------------------------------------------------------------------
-- Some list helpers
splitOn : a -> List a -> List (List a)
splitOn a ls =
  let helper a ls seen =
        case ls of
          []      -> [reverse seen]
          (x::xs) -> if x == a
                     then reverse seen :: helper a xs []      
                     else helper a xs (x::seen)
  in helper a ls []

removeFirst : a -> List a -> List a
removeFirst a ls =
  case ls of
    []      -> []
    (x::xs) -> if x == a
               then xs
               else x :: removeFirst a xs

count : a -> List a -> Int
count a ls =
  case ls of
    []      -> 0
    (x::xs) -> if a == x then count a xs + 1 else count a xs

nub : List a -> List a
nub ls =
  case ls of
    []      -> []
    (x::xs) -> if member x xs
               then nub xs
               else (x :: nub xs)


suffixes : List a -> List (List a)
suffixes ls = 
  case ls of
    []      -> []
    (x::xs) -> (x :: xs) :: suffixes xs
