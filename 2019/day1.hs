import Data.List

fuel :: Integer -> Integer
fuel m = (m `div` 3) - 2

main = getContents >>= print . sum . map (fuel . read) . words
