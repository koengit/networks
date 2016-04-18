module Aux where

-- updating an index in a list

(=!!) :: [a] -> (Int,a) -> [a]
(_:xs) =!! (0,y) = y:xs
(x:xs) =!! (k,y) = x : (xs =!! (k-1,y))

