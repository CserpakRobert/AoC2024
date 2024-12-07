module Day7 () where

main :: IO ()
main = do
  let fileName = "inputs/7.txt"
  file <- readFile fileName
  let input = map (break (== ':')) $ lines file
  let parsed = parseInput input

  print $ "firstPart: " ++ show (firstPart parsed)
  print $ "secondPart: " ++ show (secondPart parsed)

parseInput :: [([Char], [Char])] -> [(Int, [Int])]
parseInput ((a, bs) : xs)   = (read a, map read . words $ tail bs) : parseInput xs
parseInput _                = []

firstPart :: [(Int, [Int])] -> Int
firstPart = sum . map fst . filter fpValid

secondPart :: [(Int, [Int])] -> Int
secondPart = sum . map fst . filter spValid

fpValid :: (Int, [Int]) -> Bool
fpValid (y, x1 : x2 : xs)   = fpValid (y, x1 + x2 : xs) || fpValid (y, x1 * x2 : xs)
fpValid (y, [x])            = y == x
fpValid _                   = False

spValid :: (Int, [Int]) -> Bool
spValid (y, x1 : x2 : xs)   = spValid (y, x1 + x2 : xs) || spValid (y, x1 * x2 : xs) || spValid (y, (x1 * 10 ^ length (show x2) + x2) : xs)
spValid (y, [x])            = y == x
spValid _                   = False