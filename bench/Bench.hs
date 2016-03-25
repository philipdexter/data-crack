{-# LANGUAGE BangPatterns #-}

import           Control.Monad (replicateM)
import           Criterion.Main
import qualified Data.IntSet as IS
import qualified Data.Set as S
import           Data.List (foldl')
import           Test.QuickCheck

import           Data.Crack

aLAQ :: Int -> Int -> IO ([Int], [(Int, Int)])
aLAQ nl nq = do
  ls <- generate (replicateM nl arbitrary)
  nq <- map (\(l,h) -> (min l h, max l h)) <$> generate (replicateM nq arbitrary)
  return (ls, nq)

performQueries !crack !queries = foldl' (\(!c) (!l,!h) -> fst (query (Range l h) c)) crack queries

queryIntSet !is (!l, !h) = let (_, !high) = IS.split l is
                               (!middle, _) = IS.split h high
                           in middle
performQueriesC is queries = foldl' (\(!is) range -> queryIntSet is range `seq` is) is queries

querySet !is (!l, !h) = let (_, !high) = S.split l is
                            (!middle, _) = S.split h high
                        in middle
performQueriesS s queries = foldl' (\(!s) range -> querySet s range `seq` s) s queries

main = defaultMain [
        env (aLAQ 10000 0) (\ ~(ls, queries) ->
            bgroup "fromList" [ bench "crack" $ whnf Data.Crack.fromList ls
                              , bench "intset" $ whnf IS.fromList ls
                              , bench "set" $ whnf S.fromList ls
                              ]) ,
        env (aLAQ 10000 100) (\ ~(ls, queries) ->
            bgroup "compare" [ bench "crack" $ whnf (flip performQueries queries . Data.Crack.fromList) ls
                             , bench "intset" $ whnf (flip performQueriesC queries . IS.fromList) ls
                             , bench "set" $ whnf (flip performQueriesS queries . S.fromList) ls
                             ])
       ]
