{-# LANGUAGE LambdaCase #-}
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import System.Environment

distribution :: [Int] -> (Int,Int,Int)
distribution = foldl' (\(a,b,c) d -> case d of
  1 -> let a' = a + 1 in a' `seq` (a',b,c)
  2 -> let b' = b + 1 in b' `seq` (a,b',c)
  3 -> let c' = c + 1 in c' `seq` (a,b,c')
  _ -> (a,b,c)
 ) (0,0,0)

differences :: [Int] -> [Int]
differences = unfoldr (\case
  (a:r@(b:_)) -> Just (b - a, r)
  [_] -> Just (3,[])
  _ -> Nothing
 ) . (0:) . sort

arrangements :: [Int] -> Integer
arrangements l' = snd $ IM.findMax m where
  l = foldr (\a r -> case r of
    [] -> [a, a + 3]
    _ -> a:r
   ) [] $ sort l'
  m = IM.insert 0 1 $ IM.fromList $ fmap (\k ->
    (k, sum $ mapMaybe (\d ->
      IM.lookup (k - d) m
     ) [1 .. 3])
   ) l

main = do
  [fn] <- getArgs
  jrs <- (map read . words) <$> readFile fn
  let
    (s,_,t) = distribution $ differences jrs
  print $ fromIntegral s * fromIntegral t
  print $ arrangements jrs

