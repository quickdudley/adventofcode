import Data.List

fuel :: Integer -> Integer
fuel m = (m `div` 3) - 2

recursiveFuel :: Integer -> Integer
recursiveFuel = sum . takeWhile (> 0) . drop 1 . iterate fuel

main = getContents >>= print . sum . map (recursiveFuel . read) . words
