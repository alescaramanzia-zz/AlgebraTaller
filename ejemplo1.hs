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

inv ::  Float -> Float
inv x | x /= 0 = 1/x

factorial :: Integer -> Integer
factorial n | n == 0 = 1
			| n > 0 = n * ( factorial (n-1))

par :: Integer -> Bool
par 0 = True
par 1 = False
par n = par (n-2)

multiploTres :: Integer -> Bool
multiploTres n | n == 0 = True
               | n < 3 = False
               |otherwise = multiploTres (n-3)

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
			  | n > 1 = sumaImpares (n-1) + (2*n-1)

sumaImparesCuyoCuadSeaMenorQue :: Integer -> Integer
sumaImparesCuyoCuadSeaMenorQue n | n  == 1 || n == 2 = 1
								 | (n^2) < n = sumaImparesCuyoCuadSeaMenorQue n + sumaImparesCuyoCuadSeaMenorQue (n+1)
								 | otherwise = sumaImparesCuyoCuadSeaMenorQue n
								 --(2n+1)^2 < n

sumaAux :: Integer -> Integer -> Integer
sumaAux n c | n^2 >= c = 0
		    | otherwise = n + sumaAux (n+2) c

sumatoriaLista :: [Integer] -> Integer
sumatoriaLista l | length l == 0 = 0
				 |otherwise = head l + sumatoriaLista (tail l)

pertenece :: Integer -> [Integer] -> Bool
pertenece n lista | length lista == 0 = False
				  | otherwise = head lista == n || pertenece n (tail lista)

repetidos :: [Integer] -> Bool
repetidos lista | length lista == 0 = False
				| otherwise = pertenece (head lista) (tail lista) || repetidos (tail lista)

quitar :: Integer -> [Integer] -> [Integer]
quitar n lista | length lista == 0 = []
			   | head lista == n = tail lista
			   | otherwise = head lista : quitar n (tail lista)