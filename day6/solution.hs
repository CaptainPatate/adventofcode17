import Data.List (elemIndex)
import qualified Data.Sequence as S
import Debug.Trace

data Ring a = Ring Int (S.Seq a) deriving (Show, Eq)

fromList xs = Ring 0 (S.fromList xs)

selectBank :: (Ord a) => Ring a -> Ring a
selectBank (Ring i xs) = Ring (fst (S.foldrWithIndex (\i e acc -> if e >= (snd acc) then (i, e) else acc) (0, xs `S.index` 0) xs)) xs

shiftOp :: (a -> a) -> Ring a -> Ring a
shiftOp f (Ring i xs) = let nextBank = if (i + 1) == (S.length xs)
                                       then 0
                                       else i + 1
                        in Ring nextBank $ S.adjust f i xs

redistribute :: Ring Int -> Ring Int
redistribute r = let (Ring i xs) = selectBank r
                     blocks = xs `S.index` i
                     nextBlockIndex = if (i + 1) == (S.length xs)
                                      then 0
                                      else i + 1
                     red b ring
                       | b == 0 = ring
                       | otherwise = red (b - 1) (shiftOp (+1) ring)
                 in red blocks (Ring nextBlockIndex (S.update i 0 xs))

go :: [S.Seq Int] -> Ring Int -> (Int, [S.Seq Int])
go seen r = let go' c seen b@(Ring i xs) = if xs `elem` seen
                                           then (c, xs:seen)
                                           else go' (c + 1) (xs:seen) (redistribute b)
            in go' 0 seen r

readInt :: String -> Int
readInt = read

main = do
  raw <- readFile "input.txt"
  let initBank = map readInt $ words raw
      (part1, xs) = go [] (fromList initBank)
  print $ "Part 1:" ++ (show part1)
  print $ "Part 2:" ++ show ((+1) <$> (elemIndex (head xs) (tail xs)))
  
