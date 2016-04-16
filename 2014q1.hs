-- Q1 (a)

repeatCustom :: a -> [a]
repeatCustom n = [n] ++ repeatCustom n

-- Q1 (b)

replicateCustom :: Int -> a -> [a]
replicateCustom 0 _ = []
replicateCustom n a = [a] ++ replicateCustom (n-1) a

-- Q1 (c)

concatCustom :: [[a]] -> [a]
concatCustom [] = []
concatCustom (x:xs) = x ++ concatCustom xs

-- Q1 (d)

zipCustom :: [a] -> [b] -> [(a,b)]
zipCustom [] [] = []
zipCustom (x:xs) (y:ys) = [(x,y)] ++ zipCustom xs ys

-- Q1 (e)

unzipCustom :: [(a,b)] -> ([a], [b])
unzipCustom [] = ([], [])
unzipCustom xs = (mapCustom fst xs, mapCustom snd xs)

-- map
mapCustom :: (a -> b) -> [a] -> [b]
mapCustom _ [] = []
mapCustom f (x:xs) = (f x) : mapCustom f xs

-- Q1 (f)

minimumCustom :: (Ord a) => [a] -> a
minimumCustom (x:xs) = findMinimum x xs

findMinimum :: (Ord a) => a -> [a] -> a
findMinimum min [] = min
findMinimum currMin (x:xs)
  | currMin < x = findMinimum currMin xs
  | currMin > x = findMinimum x xs
  | otherwise = findMinimum currMin xs
