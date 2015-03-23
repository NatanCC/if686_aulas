dobra :: [Int] -> [Int]
dobra [] = []
dobra (x:xs) = ((2*x):(dobra xs))


sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] x = x
sumPairs x [] = x
sumPairs [] [] = []
sumPairs (x:xs) (y:ys) = (x + y): sumPairs xs ys

member :: [Int] -> Int -> Bool
member [] _ = False
member (x:xs) a | x == a = True
				| otherwise = member xs a
				
digits :: String -> String
digits [] = []
digits (x:xs) | (x >= '0') && (x <= '9') = (x:digits xs)
			| otherwise = digits xs


quickSort :: [Int] -> [Int]
quickSort [] = []
--quickSort [x] = [x]
quickSort (a:as) = (leftSplit (as) a)++[a]++(rightSplit (as) a)


leftSplit :: [Int] -> Int -> [Int]
leftSplit [] x = []
leftSplit (a:as) x | a < x = quickSort(a: (leftSplit as x))
                    | otherwise = quickSort(leftSplit as x)

rightSplit :: [Int] -> Int -> [Int]
rightSplit [] x = []
rightSplit (a:as) x | a > x = quickSort (a: (rightSplit as x))
                    | otherwise = quickSort (rightSplit as x)
