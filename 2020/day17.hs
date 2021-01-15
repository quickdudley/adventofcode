{-# LANGUAGE LambdaCase, FlexibleInstances, GADTs, ScopedTypeVariables, TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.Monoid (Endo(..))
import Data.Align
import Data.These
import Data.Hashable
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Proxy
import System.Environment

data SomeGridType where
  SomeGridType :: (Grid a, Neighbourhood a, Eq a, Hashable a) =>
    Proxy a -> SomeGridType

step :: (Neighbourhood p, Hashable p, Eq p) => S.HashSet p -> S.HashSet p
step s0 = M.keysSet $
  M.filter id $
  alignWith (\case
    This () -> False
    That n -> n == 3
    These () n -> n == 2 || n == 3
   ) (S.toMap s0) $
  M.fromListWith (+) $
  fmap (flip (,) 1) $
  S.toList s0 >>= neighbours

class Neighbourhood a where
  neighbours :: a -> [a]

instance Neighbourhood (Integer,Integer,Integer) where
  neighbours = flip map offsets . offset where
    offset ~(x1,y1,z1) ~(x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)
    offsets = filter (/= (0,0,0)) $ (,,) <$>
      [-1 .. 1] <*>
      [-1 .. 1] <*>
      [-1 .. 1]

instance Neighbourhood (Integer,Integer,Integer,Integer) where
  neighbours = flip map offsets . offset where
    offset ~(x1,y1,z1,w1) ~(x2,y2,z2,w2) = (x1 + x2, y1 + y2, z1 + z2, w1 + w2)
    offsets = filter (/= (0,0,0,0)) $ (,,,) <$>
      [-1 .. 1] <*>
      [-1 .. 1] <*>
      [-1 .. 1] <*>
      [-1 .. 1]

class Grid a where
  onRootPlane :: (Integer,Integer) -> a

instance Grid (Integer,Integer,Integer) where
  onRootPlane ~(x,y) = (x,y,0)

instance Grid (Integer,Integer,Integer,Integer) where
  onRootPlane ~(x,y) = (x,y,0,0)

raise :: Monoid a => Int -> a -> a
raise 0 _ = mempty
raise 1 a = a
raise n a = go1 1 a where
  go1 b x
    | b .&. n == 0 = go1 (shift b 1) (x <> x)
    | otherwise = go2 (shift b 1) (x <> x) x
  go2 b x s = let
    s' = if b .&. n == 0 then s else x <> s
    in if countLeadingZeros b == 0 || b >= n
      then s'
      else go2 (shift b 1) (x <> x) s'

runBoot :: (Neighbourhood p, Hashable p, Eq p) => S.HashSet p -> S.HashSet p
runBoot = appEndo $ raise 6 $ Endo step

readConfig :: (Grid p, Hashable p, Eq p) => String -> S.HashSet p
readConfig s = S.fromList $ do
  (l,y) <- zip (lines s) [0 ..]
  (c,x) <- zip l [0 ..]
  guard (c == '#')
  return $ onRootPlane (x,y)

main = do
  [d,fn] <- getArgs
  let
    gt = case d of
      "3" -> SomeGridType (Proxy @(Integer,Integer,Integer))
      "4" -> SomeGridType (Proxy @(Integer,Integer,Integer,Integer))
  case gt of
    SomeGridType (_ :: Proxy g) -> do
      c <- readConfig @g <$> readFile fn
      print $ S.size $ runBoot c

