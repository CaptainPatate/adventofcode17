import Debug.Trace

data Zipper a = Zipper [a] a [a] deriving Show

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

right :: Maybe (Zipper a) -> Maybe (Zipper a)
right Nothing = Nothing
right (Just (Zipper l a [])) = Nothing
right (Just (Zipper l a (x:xs))) = Just $ Zipper (l ++ [a]) x xs

left :: Maybe (Zipper a) -> Maybe (Zipper a)
left Nothing = Nothing
left (Just (Zipper [] _ _)) = Nothing
left (Just (Zipper l a r)) = Just $ Zipper (init l) (last l) (a:r)

composeN :: Int -> (b -> b) -> b -> b
composeN n f = foldr (.) id (replicate n f)

jump :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
jump num Nothing = Nothing
jump num z@(Just _)
  | num < 0 = (composeN (abs num) left) z
  | otherwise = (composeN num right) z

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
