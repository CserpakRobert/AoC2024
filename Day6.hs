module Day6 () where

import Data.List (elemIndex, findIndex)

data Direction = DLeft | DUp | DRight | DDown deriving (Show)

data LabData = LabData
  { rows :: Int,
    cols :: Int,
    tiles :: [String],
    currentPos :: (Int, Int)
  }

instance Show LabData where
  show :: LabData -> String
  show l@(LabData r c ts (x, y)) = show ts

main :: IO ()
main = do
  let fileName = "inputs/6.txt"
  file <- readFile fileName
  let input = lines file

  print ("first Part: " ++ show (firstPart (LabData (length input) (length $ head input) input (findStartingPoint input)) DUp))
  -- print ("second Part: " ++ show (replaceX (LabData (length input) (length $ head input) input (findStartingPoint input))))
  print ("second Part: " ++ show ((length input)* (length $ head input)))

findStartingPoint :: [String] -> (Int, Int)
findStartingPoint ss =
  let x = findIndex (elem '^') ss
   in case x of
        Nothing -> (0, 0)
        Just x ->
          let y = elemIndex '^' (ss !! x)
           in case y of
                Nothing -> (0, 0)
                Just y -> (x, y)

firstPart :: LabData -> Direction -> Int
firstPart l@(LabData r c ts (x, y)) d
  | obstacleInDirection l d =
      let newD = turnRight d
       in let nl = move l newD
           in let (fl, b) = executeMove nl in if  b then countXs fl else firstPart fl newD
  | otherwise =
      let nl = move l d
       in let (fl, b) = executeMove nl in if  b then countXs fl else firstPart fl d

replaceX :: LabData -> LabData
replaceX l@(LabData r c ts (x, y)) =
  let (as, b : bs) = splitAt x ts
   in let (cs, d : ds) = splitAt y b in LabData r c (as ++ (cs ++ 'X' : ds) : bs) (x, y)

countXs :: LabData -> Int
countXs l@(LabData r c ts (x, y)) = sum . map (length . filter (== 'X')) $ ts

executeMove :: LabData -> (LabData, Bool)
executeMove l@(LabData r c ts (x, y))
  | x < 0 || x >= r || y < 0 || y >= c = (l, True)
  | otherwise = (replaceX l, False)

move :: LabData -> Direction -> LabData
move l DLeft = let LabData r c ts (x, y) = replaceX l in LabData r c ts (x, y - 1)
move l DUp = let LabData r c ts (x, y) = replaceX l in LabData r c ts (x - 1, y)
move l DRight = let LabData r c ts (x, y) = replaceX l in LabData r c ts (x, y + 1)
move l DDown = let LabData r c ts (x, y) = replaceX l in LabData r c ts (x + 1, y)

obstacleInDirection :: LabData -> Direction -> Bool
obstacleInDirection l@(LabData r c ts (x, y)) DLeft = y > 0 && (ts !! x) !! (y - 1) == '#'
obstacleInDirection l@(LabData r c ts (x, y)) DRight = y < c-1 && (ts !! x) !! (y + 1) == '#'
obstacleInDirection l@(LabData r c ts (x, y)) DUp = x > 0 && (ts !! (x - 1)) !! y == '#'
obstacleInDirection l@(LabData r c ts (x, y)) DDown = x < r-1 && (ts !! (x + 1)) !! y == '#'

turnRight :: Direction -> Direction
turnRight DLeft = DUp
turnRight DUp = DRight
turnRight DRight = DDown
turnRight DDown = DLeft
