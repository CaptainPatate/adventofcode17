{-# LANGUAGE ViewPatterns #-}
import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.Sequence as S
import Data.Sequence (Seq, (><), viewl, ViewL((:<)))
import Prelude hiding (elem, init, round)
import Text.Printf (printf)

data Ring a = Ring {
    elem :: Seq a
  , index :: Int
  , slice :: (Int, Int)
  } deriving Show

fromList xs = Ring { elem = S.fromList xs, index = 0, slice = (0,0) }

init = fromList [0..255]

sliceLen :: Ring a -> Int
sliceLen r@(Ring _ _ (b, e)) = if b > e then
                                (ringLen r) - b + e
                              else
                                e - b

ringLen :: Ring a -> Int
ringLen = S.length . elem

updateSlice :: Int -> Ring a -> Ring a
updateSlice n r@(Ring _ i s) = r { slice = (i, sliceEnd) }
  where sliceEnd = (i+n) `mod` (ringLen r)

sliceApply :: (Seq a -> Seq a) -> Ring a -> Ring a
sliceApply f r@(Ring elem _ (b, e))
  | b > e = let (reminder, right) = S.splitAt b elem
                (left, _) = S.splitAt e elem
                middle = S.drop e reminder
                sublist = f (right >< left)
                (right', left') = S.splitAt (S.length right) sublist
             in r { elem = left' >< middle >< right' }
  | otherwise = let (left, reminder) = S.splitAt (b) elem
                    (_, right) = S.splitAt e elem
                    middle = S.take (e-b) reminder
                in r { elem = left >< f middle >< right }

reverseSelection :: Ring a -> Ring a
reverseSelection = sliceApply S.reverse

updateIndex :: Int -> Ring a -> Ring a
updateIndex skipSize r = r { index = (index r + skipSize + (sliceLen r)) `mod` (ringLen r) }

step :: Int -> Int -> Ring a -> Ring a
step skipSize n = ((updateIndex skipSize) . reverseSelection . (updateSlice n))

inputPart1 = [18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188]

run :: [Int] -> Int -> Ring Int -> (Int, Ring Int)
run input skip initialSt = foldl (\(s, lastRing) n -> (s+1, step s n lastRing)) (skip, initialSt) input

runPart1 = snd $ run inputPart1 0 init

inputPart2 = (map ord "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188") ++ [17, 31, 73, 47, 23]

round :: [Int] -> Ring Int
round input = round' input init 0 0
  where round' i st skip roundNum = if roundNum == 64 then
                                      st
                                    else
                                      let (skip', st') = run i skip st
                                      in round' i st' skip' (roundNum+1)

denseHash :: Seq Int -> Seq Int
denseHash seq = fmap (foldl xor 0) $ S.chunksOf 16 seq

toHex :: Seq Int -> String
toHex = foldl (\acc e-> acc ++ printf "%.2x" e) ""

runPart2 = toHex . denseHash . elem . round

main = let (viewl -> a :< (viewl -> b :< _)) = elem runPart1
       in do putStrLn $ "Part 1: " ++ (show (a*b))
             putStrLn $ "Part 2: " ++ (runPart2 inputPart2)
