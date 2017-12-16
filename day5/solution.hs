import Debug.Trace
import qualified Data.Sequence as S

data Zipper a = Zipper Int (S.Seq a) deriving Show

fromList :: [a] -> Zipper a
fromList xs = Zipper 0 (S.fromList xs)

right :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
right _ Nothing = Nothing
right step same@(Just (Zipper i xs))
  | step == 0 = same
  | (i + step) >= S.length xs = Nothing
  | otherwise = Just $ Zipper (i + step) xs

left :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
left _ Nothing = Nothing
left step same@(Just (Zipper i xs))
  | step == 0 = same
  | (i - step) < 0 = Nothing
  | otherwise = Just $ Zipper (i - step) xs

jump :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
jump num Nothing = Nothing
jump num z@(Just _)
  | num < 0 = left (abs num) z
  | otherwise = right num z

step :: Maybe (Zipper Int) -> Maybe (Zipper Int)
step Nothing = Nothing
step (Just (Zipper i xs)) = jump offset (Just $ Zipper i (S.adjust adjust i xs))
  where offset = (xs `S.index` i)
        adjust x = if offset >= 3 then x - 1 else x + 1

readInt :: String -> Int
readInt = read

runZipper :: Zipper Int -> Int
runZipper z = go (Just z) 0
  where
    go (Nothing) count = count
    go z@(Just (Zipper _ _)) count = if count `mod` 10000 == 0
                                       then trace (show z) $ go (step z) (count+1)
                                       else go (step z) (count+1)

main = do
  raw <- readFile "input.txt"
  let z = fromList $ map readInt $ lines raw
--  let z = Zipper [] 1 [1,1,1,0,0,0,-4,-1,0]
  print $ runZipper z
