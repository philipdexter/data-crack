{-# LANGUAGE BangPatterns #-}

module Data.Crack
( Cracked
, Range(..)
, _getV
, empty
, fromList
, toList
, query
, Data.Crack.elem
, Data.Crack.length
, Data.Crack.null
) where

import qualified Data.Vector.Unboxed as V

data Cracked = CPred !Pred !Cracked !Cracked
             | C !(V.Vector Int)
               deriving (Show, Eq)

data Range = Range !Int !Int
             deriving Show

data Pred = LessEqual !Int
            deriving (Show, Eq)

_getV :: Cracked -> V.Vector Int
_getV (C v) = v
_getV (CPred _ !vlow !vhigh) = _getV vlow V.++ _getV vhigh

empty :: Cracked
empty = C V.empty

fromList :: [Int] -> Cracked
fromList = C . V.fromList

toList :: Cracked -> [Int]
toList = V.toList . _getV

length :: Cracked -> Int
length = V.length . _getV

null :: Cracked -> Bool
null = V.null . _getV

query :: Range -> Cracked -> (Cracked, V.Vector Int)
query ran@(Range !low !high) !crack =
    case crack of
      C !v -> let (lower, higher) = V.partition (<= low) v
                  (!middle, _)    = V.partition (< high) higher
                  crack'          = CPred (LessEqual low) (C lower) (C higher)
              in (crack', middle)
      CPred (LessEqual !le) !vlow !vhigh ->
          if low > le then
              let (!down_right, !res) = query ran vhigh
              in (CPred (LessEqual le) vlow down_right, res)
          else
              if high <= le then
                  let (!down_left, !res) = query ran vlow
                  in (CPred (LessEqual le) down_left vhigh, res)
              else
                  let (!down_left, !res) = query ran vlow
                      (!down_right, !res') = query ran vhigh
                  in (CPred (LessEqual le) down_left down_right, res V.++ res')

elem :: Int -> Cracked -> Bool
elem i = not . V.null . snd . query (Range (i-1) (i+1))
