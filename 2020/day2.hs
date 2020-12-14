{-# LANGUAGE RankNTypes #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Char
import System.Environment
import System.IO

data C a = R a | F | C (Char -> C a) | B (C a) (C a)
newtype P a = P { getP :: forall b . (a -> C b) -> C b }

instance Functor P where
  fmap f (P c) = P (c . (. f))

instance Applicative P where
  pure a = P ($ a)
  P f <*> P a = P (f . (a . ) . (.))
  P a *> P b = P (a . const . b)
  P a <* P b = P (a . ((b . const) .))

instance Monad P where
  P a >>= f = P (a . flip (getP . f))

instance MonadFail P where
  fail _ = empty

instance Alternative P where
  empty = P (const F)
  P a <|> P b = P (\c -> reduce $ B (a c) (b c))

instance MonadPlus P where
  mzero = empty
  mplus = (<|>)

reduce (B F b) = b
reduce (B a F) = a
reduce (B (C a) (C b)) = C (\i -> reduce $ B (a i) (b i))
reduce c = c

runP :: P a -> String -> [a]
runP p i = extract $ go (getP p R) i where
  extract (R r) = [r]
  extract (B a b) = extract a ++ extract b
  extract _ = []
  go p [] = p
  go F _ = F
  go p (a:r) = go (step p a) r

step :: C a -> Char -> C a
step (C p) a = p a
step (B a b) i = reduce (B (step a i) (step b i))
step _ _ = F

getC :: P Char
getC = P C

peek :: P Char
peek = P (\c -> C (\i -> step (c i) i))

replace :: Char -> P ()
replace i = P (\c -> step (c ()) i)

eos :: P ()
eos = P (\c -> strip (c ())) where
  strip r@(R _) = r
  strip F = F
  strip (C p) = F
  strip (B a b) = reduce (B (strip a) (strip b))

satisfy :: (Char -> Bool) -> P Char
satisfy p = getC >>= \c -> if p c
  then return c
  else empty

munch :: (Char -> Bool) -> P String
munch p = go id where
  go acc = let
    r = acc []
    in (getC >>= \c -> if p c
      then go (acc . (c:))
      else r <$ replace c
     ) <|> (r <$ eos)

char :: Char -> P Char
char c = satisfy (== c)

number :: (Num n) => P n
number = go 0 where
  go t = do
    d <- (fromIntegral . digitToInt) <$> satisfy isDigit
    let t' = t * 10 + d
    go t' <|> return (fromInteger t')

countWithSep :: P Bool -> P s -> P Integer
countWithSep i s = let
  go n = n `seq` i >>= \i' -> let
    n' = if i' then n + 1 else n
    in (s *> go n') <|> return n'
  in go 0 <|> return 0

range :: P (Integer,Integer)
range = (,) <$> number <*> (char '-' *> number)

line :: Int -> P Bool
line v = do
  ~(lo,hi) <- range
  munch isSpace
  c <- getC
  char ':'
  munch isSpace
  case v of
    1 -> fmap ((&&) <$> (>= lo) <*> (<= hi)) $
      countWithSep (fmap (==c) $ satisfy (not . isSpace)) (return ())
    2 -> (==1) <$> cpp c 0 [lo,hi] 1
 where
  cpp _ n [] _ = n <$ munch (not . isSpace)
  cpp x n l@(c:r) p
    | c == p = getC >>= \x' -> let
      n' = if x' == x
        then n + 1
        else n
      in cpp x n' r (p + 1)
    | otherwise = getC *> cpp x n l (p + 1)

problemFile :: Int -> P Integer
problemFile v = countWithSep (line v) (char '\n') <* (return () <|> (() <$ char '\n'))

example :: String
example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

main = do
  ~[fn,v] <- getArgs
  f <- readFile fn
  let (r:_) = runP (problemFile $ read v) f
  print r

