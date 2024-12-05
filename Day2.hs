module Day2 () where

main :: IO ()
main = do
  let fileName = "inputs/2.txt"
  file <- readFile fileName
  let ls :: [[Int]] = map (map read . words) (lines file)

  print ("first Part: " ++ show (firstPart $ map distances ls))
  print ("second Part: " ++ show (secondPart $ map distances ls))

distances :: [Int] -> [Int]
distances (x1 : x2 : xs) = (x2 - x1) : distances (x2 : xs)
distances _ = []

firstPart :: [[Int]] -> Int
firstPart xs = length . filter id $ map (\a -> if head a < 0 then validNoError a 3 (< 0) else validNoError a 3 (> 0)) xs

secondPart :: [[Int]] -> Int
secondPart xs = length . filter id $ map (\a -> valid1Error a 3 (> 0) || valid1Error a 3 (< 0)) xs

validNoError :: (Num a, Ord a) => [a] -> a -> (a -> Bool) -> Bool
validNoError (x1 : xs) y p = p x1 && abs x1 <= y && validNoError xs y p
validNoError _ _ _ = True

valid1Error :: (Num a, Ord a) => [a] -> a -> (a -> Bool) -> Bool
valid1Error (x1 : x2 : x3 : xs) y p
  | p x1 && abs x1 <= y   = if p x2 && abs x2 <= y then valid1Error (x2 : x3 : xs) y p else validNoError (x1 + x2 : x3 : xs) y p || validNoError (x1 : x2 + x3 : xs) y p
  | otherwise             = validNoError (x2 : x3 : xs) y p || validNoError (x1 + x2 : x3 : xs) y p
valid1Error [x1, x2] y p  = (p x1 && abs x1 <= y) || (p x2 && abs x2 <= y)
valid1Error _ _ _ = True