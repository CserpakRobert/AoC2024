module Day9 () where
import Data.Char (digitToInt)
import Data.Maybe (isJust, isNothing)
import Data.List (nub)

main :: IO ()
main = do
  let fileName = "inputs/9.txt"
  file <- readFile fileName
  let blocks = inputToBlocks file 0
  let parTwoBlocks = blocksToPartTwoBlocks blocks


  print $ "firstPart: " ++ show (firstPart blocks )
  print $ "secondPart: " ++ show ( secondPart parTwoBlocks)

inputToBlocks :: String -> Int -> [Maybe Int]
inputToBlocks (x1:x2:xs) i= replicate (digitToInt x1) (Just i)++ replicate (digitToInt x2) Nothing ++ inputToBlocks xs (i+1)
inputToBlocks (x1:xs) i = replicate (digitToInt x1) (Just i)
inputToBlocks _ _ = []

blocksToPartTwoBlocks :: [Maybe Int]->[(Int, Maybe Int)]
blocksToPartTwoBlocks xs@(Nothing:_) = (length $ takeWhile isNothing xs,Nothing) : blocksToPartTwoBlocks (dropWhile isNothing xs)
blocksToPartTwoBlocks [] = []
blocksToPartTwoBlocks xs@(Just a:_) = (length $ takeWhile (f a) xs,Just a) : blocksToPartTwoBlocks (dropWhile (f a) xs) where
  f :: Eq a => a -> Maybe a -> Bool
  f _ Nothing = False
  f a (Just b) = a == b

firstPart :: [Maybe Int] -> Int
firstPart blocks =  sum $ zipWith (*) [0..]  (take (countJust blocks) $ firstPartdiskMap blocks (reverse blocks))

secondPart ::  [(Int, Maybe Int)] -> Int
secondPart blocks = sum . zipWith (*) [0..] . partTwoBlocksToDiskMap . filterMoved $ foldl moveToGap blocks (reverse blocks)

moveToGap :: [(Int, Maybe Int)] -> (Int, Maybe Int) -> [(Int, Maybe Int)]
moveToGap as (_,Nothing) = as
moveToGap ((ca,Nothing):as) (cb,Just b)
  | cb < ca =  (cb,Just b):(ca-cb,Nothing):as
  | cb == ca = (cb,Just b):as
  | otherwise = (ca,Nothing) : moveToGap as (cb,Just b)
moveToGap ((ca,Just a):as) (cb,Just b)
  | a == b = (ca,Just a):as
  | otherwise = (ca,Just a) : moveToGap as (cb,Just b)
moveToGap [] _ = []

partTwoBlocksToDiskMap :: [(Int, Maybe Int)] -> [Int]
partTwoBlocksToDiskMap ((ca,Nothing):as) = replicate ca 0 ++ partTwoBlocksToDiskMap as
partTwoBlocksToDiskMap ((ca,Just a):as) = replicate ca a ++ partTwoBlocksToDiskMap as
partTwoBlocksToDiskMap [] = []

filterMoved ::  [(Int, Maybe Int)] -> [(Int, Maybe Int)]
filterMoved = f [] where
  f ::  [Int] ->[(Int, Maybe Int)] -> [(Int, Maybe Int)]
  f ms ((ca,Just a):as)
    | a `elem` ms = (ca,Nothing):f ms as
    | otherwise = (ca,Just a) : f (a:ms) as
  f ms (a:as) = a: f ms as
  f _ [] = []

countJust :: [Maybe a] -> Int
countJust (Nothing:xs) = countJust xs
countJust (Just _:xs) = 1 + countJust xs
countJust _ = 0

firstPartdiskMap :: [Maybe Int] -> [Maybe Int] -> [Int]
firstPartdiskMap as (Nothing:bs) = firstPartdiskMap as bs
firstPartdiskMap (Just a:as) bs = a: firstPartdiskMap as bs
firstPartdiskMap (Nothing:as) (Just b:bs) = b: firstPartdiskMap as bs
firstPartdiskMap [] bs = []
firstPartdiskMap as [] = []
