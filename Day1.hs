module Day1 () where
import Data.Char (isSpace)
import Data.List (sort)

main :: IO ()
main = do
  let fileName = "inputs/1.txt"
  file <- readFile fileName
  let ls =  unzip $ map formatLine (lines file)

  print ("first Part: " ++ show (firstPart $ sortTupleLists ls))
  print ("second Part: " ++ show (secondPart ls))

formatLine :: [Char] -> (Int, Int)
formatLine xs =
  let x = takeWhile (not . isSpace) xs
   in let y = dropWhile (not . isSpace) (dropWhile isSpace xs)
       in (read x, read y)

sortTupleLists :: (Ord a1, Ord a2) => ([a1], [a2]) -> ([a1], [a2])
sortTupleLists (as, bs) = (sort as, sort bs)

firstPart :: (Num a) => ([a], [a]) -> a
firstPart (a : as, b : bs) = abs (a - b) + firstPart (as, bs) 
firstPart _ = 0

secondPart :: ([Int], [Int]) -> Int
secondPart (as, bs) = sum $ zipWith (*) (map (\a -> length $ filter (== a) bs) as) as
