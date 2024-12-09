module Day8 () where

import Data.List (nub)

type Point = (Int, Int)

main :: IO ()
main = do
  let fileName = "inputs/8.txt"
  file <- readFile fileName
  let input = lines file
  let xLength = length input
  let yLength = length $ head input

  print $ "firstPart: " ++ show (firstPart input xLength yLength)
  print $ "secondPart: " ++ show (secondPart (parseInput [] input (1, 1)) xLength yLength)

parseInput :: [(Char, [Point])] -> [String] -> Point -> [(Char, [Point])]
parseInput as ((x : xs) : ys) (a, b)
  | x == '.' = parseInput as (xs : ys) (a, b + 1)
  | otherwise = parseInput (addAntennaPos as x (a, b)) (xs : ys) (a, b + 1)
parseInput as ([] : ys) (a, b) = parseInput as ys (a + 1, 1)
parseInput as _ _ = as

addAntennaPos :: [(Char, [Point])] -> Char -> Point -> [(Char, [Point])]
addAntennaPos ((c, as) : xs) na np
  | c == na = (c, np : as) : xs
  | otherwise = (c, as) : addAntennaPos xs na np
addAntennaPos _ na np = [(na, [np])]

validLocation :: Int -> Int -> Point -> Bool
validLocation x y (a, b) = a > 0 && b > 0 && a <= x && b <= y

firstPart :: [String] -> Int -> Int -> Int
firstPart input x y = length . nub . filter (validLocation x y) . locations $ parseInput [] input (1, 1)

secondPart :: [(Char, [Point])] -> Int -> Int -> Int
secondPart input x y = length . nub $ moreLocations x y input ++ atleastTwoAntennaPoss input
  where
    atleastTwoAntennaPoss :: [(Char, [Point])] -> [Point]
    atleastTwoAntennaPoss ((c, ps) : as)
      | length ps >= 2 = ps ++ atleastTwoAntennaPoss as
      | otherwise = atleastTwoAntennaPoss as
    atleastTwoAntennaPoss [] = []

locations :: [(Char, [Point])] -> [Point]
locations [] = []
locations ((c, []) : xs) = locations xs
locations ((c, p : ps) : xs) = reflections p ps ++ locations ((c, ps) : xs)
  where
    reflections :: Point -> [Point] -> [Point]
    reflections p@(x, y) ((a, b) : ps) = [(2 * x - a, 2 * y - b), (2 * a - x, 2 * b - y)] ++ reflections p ps
    reflections p _ = []

moreLocations :: Int -> Int -> [(Char, [Point])] -> [Point]
moreLocations x y [] = []
moreLocations x y ((c, []) : xs) = moreLocations x y xs
moreLocations x y ((c, p : ps) : xs) = moreReflections x y p ps ++ moreLocations x y ((c, ps) : xs)
  where
    moreReflections :: Int -> Int -> Point -> [Point] -> [Point]
    moreReflections _ _ _ [] = []
    moreReflections xBound yBound p@(x, y) ((a, b) : ps) =
      takeWhile (validLocation xBound yBound) (reflections (x, y) (a, b))
        ++ takeWhile (validLocation xBound yBound) (reflections (a, b) (x, y))
        ++ moreReflections xBound yBound p ps
      where
        reflections :: Point -> Point -> [Point]
        reflections (x, y) (a, b) = let (c, d) = (2 * x - a, 2 * y - b) in (c, d) : reflections (c, d) (x, y)