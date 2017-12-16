import Debug.Trace
import qualified Data.Sequence as S

data Zipper a = Zipper (S.Seq a) a (S.Seq a) deriving Show

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper S.empty x (S.fromList xs)

right :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
right _ Nothing = Nothing
right step same@(Just (Zipper l a r))
  | step == 0 = same
  | S.null r = Nothing
  | S.length r < step = Nothing
  | otherwise = Just $ Zipper ((l S.|> a) S.>< (S.take (step - 1) r)) (r `S.index` (step - 1)) (S.drop step r)

left :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
left _ Nothing = Nothing
left step same@(Just (Zipper l a r))
  | step == 0 = same
  | S.null l = Nothing
  | len < step = Nothing
  | otherwise = Just $ Zipper (S.take (len - step) l) (l `S.index` (len - step)) ((S.drop (len - step + 1) l) S.>< (a S.<| r))
  where len = S.length l

jump :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
jump num Nothing = Nothing
jump num z@(Just _)
  | num < 0 = left (abs num) z
  | otherwise = right num z

step :: Maybe (Zipper Int) -> Maybe (Zipper Int)
step Nothing = Nothing
step (Just (Zipper l a r)) = jump a (Just $ Zipper l (a+1) r)

readInt :: String -> Int
readInt = read

runZipper :: Zipper Int -> Int
runZipper z = go (Just z) 0
  where
    go (Nothing) count = count
    go z@(Just (Zipper l a r)) count = if count `mod` 10000 == 0
                                       then trace (show z) $ go (step z) (count+1)
                                       else go (step z) (count+1)

main = do
  raw <- readFile "input.txt"
  let z = fromList $ map readInt $ lines raw
--  let z = Zipper [] 1 [1,1,1,0,0,0,-4,-1,0]
  print $ runZipper z
