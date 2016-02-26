{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

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

data Cracked = CPred {-# UNPACK #-} !Pred !Cracked !Cracked
             | C !(V.Vector Int)
               deriving (Show, Eq)

data Range = Range !Int !Int
             deriving Show

newtype Pred = LessEqual Int
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
          let (!down_right, !right_res) = if low > le || high > le then query ran vhigh else (vhigh, [])
              (!down_left, !left_res) = if low > le then (vlow, []) else query ran vlow
          in (CPred (LessEqual le) down_left down_right, qapp left_res right_res)
        where qapp [] !r = r
              qapp !l [] = l
              qapp !l !r = l V.++ r

elem :: Int -> Cracked -> Bool
elem i = not . V.null . snd . query (Range (i-1) (i+1))
