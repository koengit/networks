module Greedy where

import qualified Data.Set as S
import Data.Set( Set )
import Data.List( sortBy, sort )
import Data.Ord( comparing )

import Network

--------------------------------------------------------------------------------

greedy :: Set [Bool] -> ([Bool] -> Bool) -> ((a,a) -> (a,a)) -> [a] -> [a]
greedy forb okay sort2 xs
  | S.null forb = xs
  | otherwise   = greedy forb' okay sort2 (apply xy sort2 xs)
 where
  n = length xs
 
  (xy,forb'):_ = sortBy (comparing (\(_,s) -> S.size s)) tries
 
  tries = [ ((x,y), exec (x,y) forb)
          | x <- [0..n-1]
          , y <- [x+1..n-1]
          ]

  exec xy = S.fromList
          . filter (not . okay)
          . map (apply xy bsort2)
          . S.toList

  bsort2 (a,b) = (a `min` b, a `max` b)

--------------------------------------------------------------------------------

gsort :: ((a,a) -> (a,a)) -> [a] -> [a]
gsort sort2 xs = greedy (S.fromList vs) okay sort2 xs
 where
  n      = length xs
  okay v = sort v == v
  vs     = [ v | v <- bvecs n, not (okay v) ]

gmedian :: ((a,a) -> (a,a)) -> [a] -> [a]
gmedian sort2 xs = greedy (S.fromList vs) okay sort2 xs
 where
  n      = length xs
  i      = n `div` 2
  okay v = (sort v !! i) == (v !! i)
        && and [ v !! j <= v !! i | j <- [0..i-1] ]
        && and [ v !! j >= v !! i | j <- [i+1..n-1] ]
  vs     = [ v | v <- bvecs n, not (okay v) ]

gselect :: (Int,Int) -> ((a,a) -> (a,a)) -> [a] -> [a]
gselect (a,b) sort2 xs = greedy (S.fromList vs) okay sort2 xs
 where
  n      = length xs
  okay v = and [ sort v !! i == v !! i | i <- [a..b] ]
        && and [ v !! j <= v !! a | j <- [0..a-1] ]
        && and [ v !! j >= v !! b | j <- [b+1..n-1] ]
  vs     = [ v | v <- bvecs n, not (okay v) ]

bvecs :: Int -> [[Bool]]
bvecs 0 = [[]]
bvecs n = [ b:y | y <- bvecs (n-1), b <- [False,True] ]

--------------------------------------------------------------------------------

