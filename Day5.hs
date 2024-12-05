module Day5 () where

import Data.List ( groupBy, intersect, partition ) 

data Order = Order Int [Int] | None deriving (Show)

main :: IO ()
main = do
  let fileName = "inputs/5.txt"
  file <- readFile fileName
  let (a,b) =break (=="") $ lines file
  let (os, ls) :: ([(Int,Int)], [[Int]]) = (map parseOrder a, map (map read . splitOn ',') $ tail b)
  let orders = foldr fillOrder [] os
  let (valid, invalid) = partition (validateLine orders) ls

  print ("first Part: " ++ show (sum $ map middle valid))
  print ("second Part: " ++ show (sum (map (middle . sortInvalid orders) invalid)))

parseOrder :: String -> (Int,Int)
parseOrder s = let (a,b) = break (=='|') s in (read a, read $ tail b)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn k xs = map tail $ groupBy (/=) (k:xs)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

fillOrder :: (Int,Int) -> [Order] -> [Order]
fillOrder (x,y) [] = [Order y [x]]
fillOrder (x,y) ((Order a bs):as)
    | a == y = Order a (x:bs):as
fillOrder xy (x:xs) = x : fillOrder xy xs

findOrder :: Int -> [Order] -> Order
findOrder _ [] = None
findOrder x (o@(Order a bs):xs) | a == x = o
findOrder x (_:as) = findOrder x as

validateLine :: [Order] -> [Int] -> Bool
validateLine os (x:after) = case findOrder x os of
  None -> validateLine os after
  Order o before -> null (before `intersect` after ) && validateLine os after
validateLine _ []  = True

sortInvalid :: [Order] -> [Int] -> [Int]
sortInvalid os l@(x1:x2:xs)
  | validateLine os l = l
  | otherwise = sortInvalid os (x2: sortInvalid os (x1:xs))
sortInvalid os (x:xs) = [x]
sortInvalid _ _ = []