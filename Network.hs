module Network where

import qualified Data.Map as M
import Data.Map( Map )
import qualified Data.Set as S
import Data.Set( Set )
import Data.Ord
import Data.List( sort, transpose, sortBy )

import Aux

--------------------------------------------------------------------------------
-- applying one comparator

apply :: (Int,Int) -> ((a,a) -> (a,a)) -> [a] -> [a]
apply (x,y) sort2 xs = (xs =!! (x,a)) =!! (y,b)
 where
  (a,b) = sort2 (xs!!x,xs!!y)

--------------------------------------------------------------------------------
-- an abstract datatype that can be used to retrieve all comparators
-- from a network

data Line
  = Line Int [Int]
 deriving ( Eq, Ord, Show )

line :: Int -> Line
line x = Line x []

sort2 :: (Line,Line) -> (Line,Line)
sort2 (Line x xs, Line y ys) = (Line x (y:xs), Line y (x:ys))

--------------------------------------------------------------------------------
-- drawing the output of a (valid) network in ASCII
-- the inputs should be [line 0, line 1, .., line n]

draw :: [Line] -> IO ()
draw xs =
  do putStrLn $ unlines $ transpose $ phases
     putStrLn ( show (length cs) ++ " comparators, "
             ++ show (length chunks_cs) ++ " phases"
              )
 where
  cs = comps xs
  n  = maximum (0 : [ z+1 | (x,y) <- cs, z <- [x,y] ])
 
  chunks []         = []
  chunks ((x,y):cs) = chunk (S.fromList [x,y]) [(x,y)] cs []

  chunk forbidden ch []         cs' = sort' ch : chunks (reverse cs')
  chunk forbidden ch ((x,y):cs) cs' =
    chunk (S.insert x (S.insert y forbidden)) 
          ([ (x,y) | not no ] ++ ch)
          cs
          ([ (x,y) | no ] ++ cs')
   where
    no = x `S.member` forbidden || y `S.member` forbidden

  sort' = sortBy (comparing (\(x,y) -> (x-y,x,y)))

  mchunks []         = []
  mchunks ((x,y):cs) = mchunk (S.fromList [x..y]) [(x,y)] cs []

  mchunk forbidden ch []         cs' = sort ch : mchunks (reverse cs')
  mchunk forbidden ch ((x,y):cs) cs' =
    mchunk (S.fromList [x..y] `S.union` forbidden) 
           ([ (x,y) | not no ] ++ ch)
           cs
           ([ (x,y) | no ] ++ cs')
   where
    no = x `S.member` forbidden || y `S.member` forbidden

  drawDigits0 =
    concat
    [ [ last (show i), ' ']
    | i <- [0..n-1]
    ]

  drawDigits1 =
    concat
    [ [ if i >= 10 then last (init (show i)) else ' ', ' ']
    | i <- [0..n-1]
    ]

  drawMiniChunk cs =
    concat
    [ [ if knob
          then '+'
          else if between
                 then '|'
                 else '-'
      , if between
          then '|'
          else ' '
      ]
    | i <- [0..n-1]
    , let knob    = any (\(x,y) -> x==i || y==i) cs
          between = any (\(x,y) -> x <= i && i < y) cs
    ]

  chunks_cs = chunks cs

  phases =
    [ drawDigits1
    , drawDigits0
    , drawMiniChunk []
    , drawMiniChunk []
    ] ++
    concat
    [ [ drawMiniChunk cs
      | cs <- mchunks ch
      ] ++
      [ drawMiniChunk []
      , drawMiniChunk []
      ]
    | ch <- chunks_cs
    ] ++
    [ drawDigits1
    , drawDigits0
    ]

--------------------------------------------------------------------------------
-- calculating all comparators in a (valid) network

comps :: [Line] -> [(Int,Int)]
comps xs = get [] tab0
 where
  tab0 = M.fromList [ (x, ys) | Line x ys@(_:_) <- xs ]

  get cs tab
    | M.null tab = cs
    | null cs'   = error ("not a valid network: " ++ show tab)
    | otherwise  = get (cs'++cs) (foldr shift tab [ z | (x,y) <- cs', z <- [x,y] ])
   where
    xys = M.toList tab
    cs' = [ (x,y)
          | (x,y:ys) <- xys
          , x < y
          , Just (x':xs) <- [M.lookup y tab]
          , x == x'
          ]
    
    shift x tab = M.update mtail x tab

    mtail []     = Nothing
    mtail [_]    = Nothing
    mtail (_:xs) = Just xs

--------------------------------------------------------------------------------

