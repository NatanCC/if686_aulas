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