suma x y = x + y

doble x = 2*x

normaVectorial v1 v2 = sqrt(v1^2 + v2^2)

funcionConstante x = 8

respuestaATodo = 42

delta n | otherwise = 0 -- para este caso se puede usar "otherwise" y evalua todos los otros casos posibles
        | n == 0 = 1

signo x | x == 0 = 0
        | x > 0 = 1
        | otherwise = -1
    
maximo x y | x > y = x
           | otherwise = y

maximo3 x y z = maximo (maximo x y ) z

--absoluto x | x > 0 = x
--           | otherwise = x + (-2*x)

absoluto x = x * signo(x)

f n1 n2 n3 | n2 < 10 = n1
           | otherwise = n2 + n3

--cuadratica a b c | sqrt(b**2 - 4*a*c) < 0 = 

esPar :: Integer -> Bool
esPar n | mod n 2 == 0 = True
		| otherwise = False

esPositiva :: Float -> Bool
esPositiva x | x >= 0 = True
			 | otherwise = False

resultadoCurso :: Float -> Float -> Float -> String
resultadoCurso n1 n2 tp | (n1+n2+tp)/3 >= 7 = "Promocionado"
						| (n1+n2+tp)/3 < 4 = "No Aprobado"
						| otherwise = "A final"

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x1,y1) (x2, y2) = sqrt((x2-x1)^2+(y2-y1)^2)