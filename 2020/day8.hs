{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Either
import Data.Maybe
import Data.List (intercalate)
import Data.Monoid
import qualified Data.IntMap as IM
import qualified Data.Set as S
import System.Environment

data Operation = ACC | JMP | NOP deriving (Eq,Show)

pcIncrement :: Operation -> Int -> Int
pcIncrement JMP i = i
pcIncrement _ _ = 1

accIncrement :: Operation -> Int -> Int
accIncrement ACC i = i
accIncrement _ _ = 0

runBoot :: IM.IntMap (Operation,Int) -> [(Int,Int)]
runBoot b = case IM.lookupMin b of
  Nothing -> []
  Just (pc0,_) -> go pc0 0
 where
  go pc acc = case IM.lookup pc b of
    Nothing -> []
    Just (op,i) -> let
      pc' = pc + pcIncrement op i
      acc' = acc + accIncrement op i
      in (pc', acc') : go pc' acc'

repairAttempts :: Alternative f =>
  IM.IntMap (Operation, Int) -> f (IM.IntMap (Operation, Int))
repairAttempts = alterEach $ \(opc,i) -> case opc of
  ACC -> empty
  NOP -> pure $ Just (JMP,i)
  JMP -> pure $ Just (NOP,i)

alterEach :: Alternative f =>
  (a -> f (Maybe a)) -> IM.IntMap a -> f (IM.IntMap a)
alterEach f m = getAlt $
  IM.foldMapWithKey (\k _ -> Alt $ IM.alterF (f . fromJust) k m) m

accBeforeLoop :: [(Int,Int)] -> Maybe Int
accBeforeLoop = go (S.singleton 0) . ((0,0):) where
  go v ((_,acc):r@((pc,_):_)) = either Just (flip go r) $ S.alterF (\p -> if p then Left acc else Right True) pc v
  go _ _ = Nothing

accOnComplete :: Int -> [(Int,Int)] -> Maybe Int
accOnComplete t = go S.empty where
  go _ [] = Nothing
  go _ [(pc,acc)]
    | pc == t = Just acc
    | otherwise = Nothing
  go v ((pc,acc):r)
    | pc `S.member` v = Nothing
    | otherwise = go (S.insert pc v) r

anyChar :: StateT String [] Char
anyChar = StateT (\case
  [] -> []
  (a:r) -> [(a,r)]
 )

string :: String -> StateT String [] ()
string [] = return ()
string (a:r) = anyChar >>= \c -> if a == c
  then string r
  else empty

opcode :: StateT String [] Operation
opcode = (ACC <$ string "acc") <|>
  (JMP <$ string "jmp") <|>
  (NOP <$ string "nop")

natural :: Num a => StateT String [] a
natural = go 0 where
  go acc = anyChar >>= \c -> if isDigit c
    then let
      acc' = acc * 10 + fromIntegral (digitToInt c)
      in acc' `seq` return acc' <|> go acc'
    else empty

eos :: StateT String [] ()
eos = StateT (\case
  [] -> [((),[])]
  _ -> []
 )

neos :: StateT String [] ()
neos = StateT (\case
  [] -> []
  _ -> [((),[])]
 )

line :: StateT String [] (Operation,Int)
line = (,) <$>
  (opcode <* string " ") <*>
  (
    ((id <$ string "+") <|> (negate <$ string "-")) <*>
    natural
  )

sepBy :: Alternative m => m a -> m s -> m [a]
sepBy a s = pure [] <|>
  ((:) <$> a <*> many (s *> a))

file :: StateT String [] (IM.IntMap (Operation,Int))
file = (IM.fromList . zip [0..]) <$> sepBy line (string "\n") <* (pure () <|> void (string "\n"))

main = do
  (part:fn:_) <- getArgs
  (b:_) <- evalStateT (file <* eos) <$> readFile fn
  print $ case part of
    "1" -> fromJust $ accBeforeLoop $ runBoot b
    "2" -> let
      gc = accOnComplete $ IM.size b
      in head $ do
        b' <- repairAttempts b
        maybeToList $ gc $ runBoot b'

showOps :: IM.IntMap (Operation,Int) -> String
showOps = intercalate "\n" . map (\(op,i) ->
  map toLower (show op) ++
  " " ++
  (if i < 0 then "-" else "+") ++
  show (abs i)
 ) . IM.elems
