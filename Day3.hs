module Day3 (main) where
import Text.Regex.Posix



main :: IO ()
main = do
  let fileName = "inputs/3.txt"
  file <- readFile fileName
  let ls = lines file
  let test = head ls

  print ("first Part: " ++ show (firstPart ls))
  print ("second Part: " ++ show (secondPart ls))

mulRegex :: String
mulRegex = "mul\\(([0-9]+),([0-9]+)\\)"

firstPart :: [String] -> Int
firstPart = sum . map ((sum . map executeMul) . validMuls)

secondPart :: [String] -> Int
secondPart = (sum . map executeMul) . validDoMuls . foldr1 (++) 

validMuls :: String -> [String]
validMuls s = getAllTextMatches (s =~ mulRegex)

validDoMuls :: String -> [String]
validDoMuls s = f $ getAllTextMatches (s =~ "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)") where
  f :: [String] -> [String]
  f xs@(y : ys) =
    let (valid, rest) = break (== "don't()") xs
    in let (invalid, rest2) = break (== "do()") rest
        in valid ++ f (tailOrEmpty rest2)
  f [] = []

tailOrEmpty :: [a] -> [a]
tailOrEmpty (x : xs) = xs
tailOrEmpty [] = []

executeMul :: String -> Int
executeMul s
  | s =~ mulRegex =
      let numS :: [String] = getAllTextMatches (s =~ "[0-9]+")
       in product $ map read numS
  | otherwise = 0
