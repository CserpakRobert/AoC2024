module Day11 () where

main :: IO ()
main = do
  let fileName = "inputs/11.txt"
  file <- readFile fileName
  let input :: [Int] = map read $ words . head . lines $ file
  print $ "first Part:" ++ show (firstpart input 25)


firstpart :: (Integral a, Show a) => [a] -> Int -> Int
firstpart xs x = length $ iterate (foldMap f) xs !! x

f :: (Integral a, Show a) => a -> [a]
f 0 = [1]
f x =
    let l = length $ show x
    in if even l
        then
            let l2 = (10 ^ (l `div` 2))
            in [x `div` l2, x `mod` l2]
        else [x * 2024]

