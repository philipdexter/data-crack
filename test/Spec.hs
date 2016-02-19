{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Data.List (sort)
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Data.Crack
import qualified Data.Vector.Unboxed as V

instance Arbitrary Range where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Range (min a b) (max a b))

instance Arbitrary Cracked where
    arbitrary = fst <$> (arbitrary :: Gen (Cracked, [Int]))

instance Arbitrary (Cracked, [Int]) where
    arbitrary = sized arbitrary'
        where
          arbitrary' numQueries = do
            (xs::[Int]) <- arbitrary
            (,xs) <$> foldM (\c _ -> do
                               r <- arbitrary
                               return . fst . query r $ c
                            ) (fromList xs) [1..numQueries]

main :: IO ()
main = defaultMain tests

tests = [
 testGroup "main" [
                testProperty "sameElems (fromList . toList) id" (sameElemsF (fromList . toList) id),
                testProperty "exclusive ranges" exclusiveRange,
                testProperty "ranges are in range" rangeProp,
                testProperty "found in initial" allIn,
                testProperty "all queried in cracked" allQueriedAreIn,
                testProperty "query doesn't affect cracked" allInAfterQuery
               ]
 ]

sameElems c1 c2 = let getElems = sort . toList
                  in getElems c1 === getElems c2

sameElemsF f g = \c -> sameElems (f c) (g c)

exclusiveRange = \c i -> let r = Range i (i + 1)
                         in V.toList (snd (query r c)) === []

rangeProp = \c a b -> let low = min a b
                          high = max a b
                          r = Range low high
                      in conjoin [ x > low && x < high  | x <- V.toList (snd (query r c))]

allIn = \(c, xs) -> conjoin [ x `Data.Crack.elem` c | x <- xs ] .&&. conjoin [ x `Prelude.elem` xs | x <- toList c ]

allQueriedAreIn = \c r -> let (c', q) = query r c
                          in conjoin [ x `Data.Crack.elem` c && x `Data.Crack.elem` c' | x <- V.toList q ]

allInAfterQuery = \c r -> let (c', _) = query r c
                          in sameElems c c'
