import Data.List

ana :: String -> [String]
ana s = permutations s
                    
isAna :: [String] -> [String] -> Bool
isAna a b = any (`elem` b) a

dupAna :: Bool -> [String] -> Bool
dupAna False [] = False
dupAna True _ = True
dupAna False (x:xs) = dupAna (isAna (ana x) xs) xs

duplicate :: String -> Bool
duplicate xs = nub (words xs) == words xs

valid pass = length (nub $ sort $ map sort pass) == length pass

main = do
       passphrases <- readFile "input.txt"
       --myDupsRaw <- readFile "dup.txt"
       --let myDups = lines myDupsRaw
       --let wrongDups = (map valid . map words . lines) passphrases
       --writeFile "dup_him.txt" $ unlines $ map snd $ filter (not . fst) $ zip wrongDups (lines passphrases)
       --let dup = map duplicate (lines passphrases)
       let dup = filter (not . dupAna False) (map words $ lines passphrases)
       print $ length dup
       --writeFile "dup.txt" $ unlines $ map (show . unwords) dup
       print $ (length . filter valid . map words . lines) passphrases
