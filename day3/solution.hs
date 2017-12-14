outer :: Int -> [Int]
outer x = []

intPow :: (Floating a, Integral c, RealFrac a) => a -> c
intPow = round . (**2)

squares :: [Int]
squares = take 1000 $ map snd $ filter fst $ zip (cycle [True,False]) (map (intPow) [1,2..])

bounds :: [Int] -> Int -> (Int, Int)
bounds xs e = (last $ filter (<e) xs, head (filter (>e) xs))

outerNum :: (Int, Int) -> [Int]
outerNum (low, high) = [low+1..high]

intSqrt :: (Floating a, Integral c, RealFrac a) => a -> c
intSqrt = round . sqrt

maxHop :: (Int, Int) -> Int
maxHop (_, high) = (intSqrt high) - 1

hops :: (Int, Int) -> [Int]
hops b = let semiHop = [(round ((maxHop b) / 2)..(maxHop b)] :: [Int]
  in cycle $ (reverse $ tail semiHop) ++ (init semiHop)

main = do
  let input = 312051
  let b = bounds squares input
  let nums = outerNum b
  let h = hops b
  print $ filter (\x -> (fst x) == input) $ zip (reverse nums) h
  
