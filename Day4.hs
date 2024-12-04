module Day4 (main) where

import Data.List (isPrefixOf)

data CrossWordData = CWD
  { rowLength :: Int,
    colLength :: Int,
    letters :: String,
    remainingCount :: Int,
    currentPos :: Int
  }

main :: IO ()
main = do
  let fileName = "inputs/4.txt"
  file <- readFile fileName
  let input = lines file
  let xLength = length $ head input
  let yLength = length input
  let cwd = CWD xLength yLength (foldr1 (++) input) (xLength * yLength) 0

  print ("first Part: " ++ show (firstPart cwd))
  print ("second Part: " ++ show (secondPart cwd))


firstPart :: CrossWordData -> Int
firstPart cwd@(CWD x y (l : ls) r c)
  | r < 4     = 0
  | otherwise = countxmas cwd + firstPart (CWD x y ls (r - 1) (c + 1))
firstPart _   = 0

secondPart :: CrossWordData -> Int
secondPart cwd@(CWD x y (l : ls) r c)
  | r < 2*x   = 0
  | otherwise = xMas cwd + secondPart (CWD x y ls (r - 1) (c + 1))
secondPart _  = 0

countxmas :: CrossWordData -> Int
countxmas cwd = diagonal cwd + horizontal cwd + vertical cwd

horizontal :: CrossWordData -> Int
horizontal (CWD x y l r c)
  | c `mod` x < x - 3 && ("XMAS" `isPrefixOf` l || "SAMX" `isPrefixOf` l) = 1
  |                                                             otherwise = 0

vertical :: CrossWordData -> Int
vertical (CWD x y l@('X' : _) r c)
  | r >= x * 3  = if (l !! x) == 'M' && (l !! (2 * x)) == 'A' && (l !! (3 * x)) == 'S' then 1 else 0
  | otherwise   = 0
vertical (CWD x y l@('S' : _) r c)
  | r >= x * 3  = if (l !! x) == 'A' && (l !! (2 * x)) == 'M' && (l !! (3 * x)) == 'X' then 1 else 0
  | otherwise   = 0
vertical _      = 0

diagonal :: CrossWordData -> Int
diagonal cwd = diagonal1 cwd + diagonal2 cwd
  where
    diagonal1 :: CrossWordData -> Int
    diagonal1 cwd@(CWD x y ('X' : _) r c)
      | r - 1 >= x * 3 + 3 && c `mod` x < x - 3 = d1 cwd
      |                               otherwise = 0
    diagonal1 cwd@(CWD x y ('S' : _) r c)
      | r - 1 >= x * 3 + 3 && c `mod` x < x - 3 = d2 cwd
      |                               otherwise = 0
    diagonal1 _ = 0
    diagonal2 :: CrossWordData -> Int
    diagonal2 cwd@(CWD x y ('X' : _) r c)
      | c `mod` x >= 3 && r - 1 >= x * 3 = d3 cwd
      |                        otherwise = 0
    diagonal2 cwd@(CWD x y ('S' : _) r c)
      | c `mod` x >= 3 && r - 1 >= x * 3 = d4 cwd
      |                        otherwise = 0
    diagonal2 _ = 0
    d1 (CWD x y l r c) = if (l !! (x + 1)) == 'M' && (l !!  (2 + 2 * x)) == 'A' && (l !! (3 + 3 * x))  == 'S' then 1 else 0
    d2 (CWD x y l r c) = if (l !! (x + 1)) == 'A' && (l !!  (2 + 2 * x)) == 'M' && (l !! (3 + 3 * x))  == 'X' then 1 else 0
    d3 (CWD x y l r c) = if (l !! (x - 1)) == 'M' && (l !! (-2 + 2 * x)) == 'A' && (l !! (-3 + 3 * x)) == 'S' then 1 else 0
    d4 (CWD x y l r c) = if (l !! (x - 1)) == 'A' && (l !! (-2 + 2 * x)) == 'M' && (l !! (-3 + 3 * x)) == 'X' then 1 else 0


xMas :: CrossWordData -> Int
xMas (CWD x y l@('M':_) r c) 
    | c `mod` x < x - 2 && r > 2*x && (l !! 2) == 'M' = if  (l !! (x + 1)) == 'A' && (l !! (2 * x)) == 'S'  && (l !! (2 * x+2)) == 'S' then 1 else 0
    | c `mod` x < x - 2 && r > 2*x && (l !! 2) == 'S' = if  (l !! (x + 1)) == 'A' && (l !! (2 * x)) == 'M'  && (l !! (2 * x+2)) == 'S' then 1 else 0
    | otherwise = 0
xMas (CWD x y l@('S':_) r c) 
    | c `mod` x < x - 2 && r > 2*x && (l !! 2) == 'S' = if  (l !! (x + 1)) == 'A' && (l !! (2 * x)) == 'M'  && (l !! (2 * x+2)) == 'M' then 1 else 0
    | c `mod` x < x - 2 && r > 2*x && (l !! 2) == 'M' = if  (l !! (x + 1)) == 'A' && (l !! (2 * x)) == 'S'  && (l !! (2 * x+2)) == 'M' then 1 else 0
    | otherwise = 0
xMas _ = 0
