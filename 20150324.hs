menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c | a <= b && a <= c &&  b >= c = (a, b)
				| b <= a && b <= c &&  a >= c = (b, a)
				| c <= a && c <= b &&  a >= c = (c, a)
				| a <= b && a <= c &&  c >= b = (a, c)
				| b <= a && b <= c &&  c >= a = (b, c)
				| otherwise = (c, b)
				
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) | a >= b && b >= c = (c, b, a)
						| a >= c && c >= b = (b, c, a)
						| b >= a && a >= c = (c, a, b)
						| b >= c &&  c >= a = (a, c, b)
						| c >= a && a >= b = (b, a, c)
						| otherwise = (a, b, c)
						
						
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)


firstCoord :: Ponto -> Float
firstCoord (a, b) = a

secondCoord :: Ponto -> Float
secondCoord (a, b) = b

isVertical :: Reta -> Bool
isVertical ((x1, y1), (x2, y2)) | x1 == x2 = True
								| otherwise = False
								
pontoY :: Float -> Reta -> Float
pontoY a ((x1, y1), (x2, y2)) = ((y2*a)-(y2*x1)-(y1*a)+(y1*x2))/(x2-x1)
