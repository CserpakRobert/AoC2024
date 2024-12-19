module Day17 () where

import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Control.Monad
import Data.Char (intToDigit)

--                  A   B   C   Program InstPointer
data Register = Reg Int Int Int [Int]   Int         deriving (Show)

testReg1, testReg2, testReg3, testReg4, testReg5, testReg6, defReg, testReg7 :: Register
testReg1 = Reg 0 0 9 [2,6] 0
testReg2 = Reg 10 0 0 [5,0,5,1,5,4] 0
testReg3 = Reg 2024 0 0 [0,1,5,4,3,0] 0
testReg4 = Reg 0 29 0 [1,7] 0
testReg5 = Reg 0 2024 43690 [4,0] 0
testReg6 = Reg 729 0 0 [0,1,5,4,3,0] 0
testReg7 = Reg 117440 0 0 [0,3,5,4,3,0] 0
defReg =   Reg 30886132 0 0 [2,4,1,1,7,5,0,3,1,4,4,4,5,5,3,0] 0
defReg2 =   Reg 30886132 0 0 [2,4,1,1,7,5,0,3,1,4,4,4,5,5,3,0] 0
main :: IO ()
main = do
  let fileName = "inputs/13test.txt"
  file <- readFile fileName

  print $ "test:" ++ show (runProgram testReg7)
--   print $ "firstPart:" ++ show (snd $ runProgram defReg2)
  print $ "firstPart:" ++ show ( snd $ runProgram defReg2)


runProgram :: Register -> (((), Register), [Int])
runProgram prog =  runWriterT (runStateT executeInstructions prog) []

executeInstructions :: (MonadState Register m, MonadWriter [Int] m) => m ()
executeInstructions = executeInstruction >> programFinished >>= \pf -> unless pf executeInstructions

programFinished :: (MonadState Register m, MonadWriter [Int] m) => m Bool
programFinished = do
    (Reg a _ _ ps p) <- get
    return . not $ length ps > p + 1  || ( a /= 0 && last ps == 3  )

executeInstruction :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
executeInstruction =  do
    i <- getNextInst
    case i of
        0 -> adv
        1 -> bxl
        2 -> bst
        3 -> jnz
        4 -> bxc
        5 -> out
        6 -> bdv
        7 -> cdv
    where
    adv :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
    adv = do
        combo <- getNextInst >>= getValue
        areg <- getValue 4
        modifyReg 4 (const $ areg `div` (2^combo))
    bxl :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
    bxl = do
        lit <- getNextInst
        breg <- getValue 5
        modifyReg 5 (const $ lit `xor` breg)
    bst :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
    bst = do
        combo <- getNextInst >>= getValue
        modifyReg 5 (const $ combo `mod` 8)
    jnz :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
    jnz = do
        aReg <- getValue 4
        unless (aReg == 0) $  getNextInst >>= \lit -> modifyPointer (const lit)
    bxc :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
    bxc = do
        combo <- getNextInst >>= getValue
        breg <- getValue 5
        creg <- getValue 6
        modifyReg 5 (const $ breg `xor` creg)
    out :: (MonadState Register m, MonadWriter [Int] m) => m()
    out = do
        combo <- getNextInst >>= getValue
        tell [combo `mod` 8]
    bdv :: (MonadState Register m, MonadWriter [Int] m) => m()
    bdv = do
        combo <- getNextInst >>= getValue
        areg <- getValue 4
        modifyReg 5 (const $ areg `div` (2^combo))
    cdv :: (MonadState Register m, MonadWriter [Int] m) => m()
    cdv = do
        combo <- getNextInst >>= getValue
        areg <- getValue 4
        modifyReg 6 (const $ areg `div` (2^combo))

modifyReg :: (MonadState Register m, MonadWriter [Int] m) => Int -> (Int -> Int) ->  m ()
modifyReg r f = do
    (Reg a b c ps p) <- get
    case r of
        4 -> put (Reg (f a) b c ps p)
        5 -> put (Reg a (f b) c ps p)
        6 -> put (Reg a b (f c) ps p)
        _ -> put (Reg a b c ps p)

getNextInst :: (MonadState Register m, MonadWriter [Int] m) => m Int
getNextInst = do
    (Reg a b c ps p) <- get
    let inst = ps !! p
    advancePointer
    return inst

advancePointer :: (MonadState Register m, MonadWriter [Int] m) =>  m ()
advancePointer = modifyPointer (+1)

modifyPointer :: (Int -> Int) ->(MonadState Register m, MonadWriter [Int] m) =>  m ()
modifyPointer f= do
    (Reg a b c ps p) <- get
    put $ Reg a b c ps (f p)

getValue ::(MonadState Register m, MonadWriter [Int] m) => Int -> m Int
getValue x
    | x >= 0 && x <= 3 = return x
    | x >= 7 = undefined
    | otherwise = getReg x where
    getReg :: (MonadState Register m, MonadWriter [Int] m) => Int -> m Int
    getReg i = do
        (Reg a b c _ _) <- get
        case i of
            4 -> return a
            5 -> return b
            6 -> return c
            _ -> undefined


