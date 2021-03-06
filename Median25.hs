module Median25 where

import Network
import Aux

--------------------------------------------------------------------------------

printMedian25 :: IO ()
printMedian25 = draw (median25 [line i | i <- [0..24]])

--------------------------------------------------------------------------------

median25 :: [Line] -> [Line]
median25 xs = go median25comps xs
 where
  go []      xs = xs
  go (xy:cs) xs = go cs (apply xy sort2 xs)

-- taken from page 13/14 of http://ndevilla.free.fr/median/median.pdf
median25comps :: [(Int,Int)]
median25comps =
  [ (0, 1)
  , (3, 4)
  , (2, 4)
  , (2, 3)
  , (6, 7)
  , (5, 7)
  , (5, 6)
  , (9, 10)
  , (8, 10)
  , (8, 9)
  , (12, 13)
  , (11, 13)
  , (11, 12)
  , (15, 16)
  , (14, 16)
  , (14, 15)
  , (18, 19)
  , (17, 19)
  , (17, 18)
  , (21, 22)
  , (20, 22)
  , (20, 21)
  , (23, 24)
  , (2, 5)
  , (3, 6)
  , (0, 6)
  , (0, 3)
  , (4, 7)
  , (1, 7)
  , (1, 4)
  , (11, 14)
  , (8, 14)
  , (8, 11)
  , (12, 15)
  , (9, 15)
  , (9, 12)
  , (13, 16)
  , (10, 16)
  , (10, 13)
  , (20, 23)
  , (17, 23)
  , (17, 20)
  , (21, 24)
  , (18, 24)
  , (18, 21)
  , (19, 22)
  , (8, 17)
  , (9, 18)
  , (0, 18)
  , (0, 9)
  , (10, 19)
  , (1, 19)
  , (1, 10)
  , (11, 20)
  , (2, 20)
  , (2, 11)
  , (12, 21)
  , (3, 21)
  , (3, 12)
  , (13, 22)
  , (4, 22)
  , (4, 13)
  , (14, 23)
  , (5, 23)
  , (5, 14)
  , (15, 24)
  , (6, 24)
  , (6, 15)
  , (7, 16)
  , (7, 19)
  , (13, 21)
  , (15, 23)
  , (7, 13)
  , (7, 15)
  , (1, 9)
  , (3, 11)
  , (5, 17)
  , (11, 17)
  , (9, 17)
  , (4, 10)
  , (6, 12)
  , (7, 14)
  , (4, 6)
  , (4, 7)
  , (12, 14)
  , (10, 14)
  , (6, 7)
  , (10, 12)
  , (6, 10)
  , (6, 17)
  , (12, 17)
  , (7, 17)
  , (7, 10)
  , (12, 18)
  , (7, 12)
  , (10, 18)
  , (12, 20)
  , (10, 20)
  , (10, 12)
  ]

--------------------------------------------------------------------------------

