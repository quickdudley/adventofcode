import Control.Monad
import Data.List
import qualified Data.Set as S
import System.Environment

stretches :: Int -> [a] -> [([a],a)]
stretches n l = let
  (i,z) = splitAt n l
  go _ [] = []
  go p (a:r) = (p,a) : go (drop 1 p ++ [a]) r
  in go i z

valid p n = not $ null $ do
  (a:r) <- tails $ sort $ filter (< n) p
  let t = n - a
  (_,ge) <- [break (>= t) r]
  (_:_) <- [takeWhile (== t) ge]
  return ()

part1 :: (Num n, Ord n) => [n] -> n
part1 = snd . head . filter (not . uncurry valid) . stretches 25

stretchesWithSum :: (Num a, Ord a) => a -> [a] -> [[a]]
stretchesWithSum t = go 0 [] where
  go es e r = case es `compare` t of
     LT -> case r of
       (h:r') -> go (es + h) (e ++ [h]) r'
       [] -> []
     EQ -> e : let
       (d:r') = e
       in go (es - d) r' r
     GT -> let
       (d:r') = e
       in go (es - d) r' r

part2 :: (Num n, Ord n) => [n] -> n -> n
part2 l e = head $ do
  l'@(_:_:_) <- stretchesWithSum e l
  Just (MinMax a h) <- pure $ foldMap (Just . minMax) l'
  return (a + h)

data MinMax a = MinMax a a

instance Ord a => Semigroup (MinMax a) where
  MinMax l1 h1 <> MinMax l2 h2 = MinMax (min l1 l2) (max h1 h2)

minMax :: a -> MinMax a
minMax = join MinMax

main :: IO ()
main = do
  [fn] <- getArgs
  d <- (map (read :: String -> Integer) . words) <$> readFile fn
  let p1 = part1 d
  print p1
  print $ part2 d p1

