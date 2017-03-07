--Trabajo Practico -Parte 1

--Definiciones propias

data Mensaje = TextoClaro Texto | CifradoReverso Mensaje  
        deriving (Eq , Show)

type Texto = [Char]

--Funciones auxiliares

esPosibleMensaje :: Texto -> Bool --Verifica que los caracteres ingresados en el string concuerden con los válidos en el TP usando la función elem para 
                                  --filtrar las mayúsculas y el espacio (se crea una lista que contenga todos los caracteres posibles para cotejar).
esPosibleMensaje []                                    = False
esPosibleMensaje [x]    | x `elem` [' ']++['A'..'Z']   = True
esPosibleMensaje (x:xs) | x `elem` [' ']++['A'..'Z']   = esPosibleMensaje xs
                        | otherwise                    = False
--Ejercicio 1--

crearMensaje :: Texto -> Mensaje
crearMensaje xs | (esPosibleMensaje xs) == True = TextoClaro xs
                | otherwise                     = (TextoClaro "ERROR") 

--Ejercicio 2--

esMensajeCifrado :: Mensaje -> Bool
esMensajeCifrado (TextoClaro a)                  = False
esMensajeCifrado (CifradoReverso (TextoClaro a)) = True 

--Ejercicio 3--

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro a)                  = CifradoReverso (TextoClaro (reverse a))
cifrarReverso (CifradoReverso (TextoClaro a)) = CifradoReverso (TextoClaro (reverse a))

--Ejercicio 4--

extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro a)                  = (reverse a)
extraerMensajeParaEnvio (CifradoReverso (TextoClaro a)) = a

--Ejercicio 5--

descifrar :: Mensaje -> Texto
descifrar (TextoClaro a)                  = a
descifrar (CifradoReverso (TextoClaro a)) = (reverse a)

--Ejercicio 6-- 

esAptoReverso :: Mensaje -> Bool
esAptoReverso (TextoClaro a)                  | ((a) == (reverse a))                = False
                                              | otherwise                           = True
esAptoReverso (CifradoReverso (TextoClaro a)) | ((a) == (reverse a))                = False
                                              | otherwise                           = True

--Ejemplos de ejecución--

--  11   master  ✔  ghci                              ↑ ✱  ~/g/AlgebraTaller 
-- GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
-- Loading package ghc-prim ... linking ... done.
-- Loading package integer-gmp ... linking ... done.
-- Loading package base ... linking ... done.
-- Prelude> :l TP1
-- [1 of 1] Compiling Main             ( TP1.hs, interpreted )
-- *Main> crearMensaje "PRIMERA PRUEBA"
-- TextoClaro "PRIMERA PRUEBA"
-- *Main> crearMensaje " -PRIMERA PRUEBA"
-- TextoClaro "ERROR"
-- *Main> crearMensaje "92PRIMERA PRUEBA"
-- TextoClaro "ERROR"
-- *Main> esMensajeCifrado (TextoClaro "PRIMERA PRUEBA")
-- False
-- *Main> cifrarReverso (TextoClaro "PRIMERA PRUEBA")
-- CifradoReverso (TextoClaro "ABEURP AREMIRP")
-- *Main> esMensajeCifrado (CifradoReverso (TextoClaro "ABEURP AREMIRP"))
-- True
-- *Main> extraerMensajeParaEnvio (TextoClaro "PRIMERA PRUEBA")
-- "ABEURP AREMIRP"
-- *Main> extraerMensajeParaEnvio (CifradoReverso (TextoClaro "ABEURP AREMIRP"))
-- "ABEURP AREMIRP"
-- *Main> descifrar (TextoClaro "PRIMERA PRUEBA")
-- "PRIMERA PRUEBA"
-- *Main> descifrar (CifradoReverso (TextoClaro "ABEURP AREMIRP"))
-- "PRIMERA PRUEBA"
-- *Main> esAptoReverso (TextoClaro "PRIMERA PRUEBA")
-- True
-- *Main> esAptoReverso (CifradoReverso (TextoClaro "ABEURP AREMIRP"))
-- True
-- *Main> esAptoReverso (TextoClaro "FIBRON")
-- True
-- *Main> esAptoReverso ( cifrarReverso ( cifrarReverso ( crearMensaje " OJOTA") ) )
-- True
-- *Main> esAptoReverso ( cifrarReverso ( cifrarReverso ( crearMensaje " RECONOCER ") ) )
-- False

--Bibliografía utilizada

--http://www.haskell.org/hoogle/ (para buscar expresiones y funciones en detalle).
--Diapositivas del taller
--http://aprendehaskell.es/

-- Miraglia, Jorge - 666/17 - Jorge10192@live.com.ar
-- Arriondo, Alexis - 234/17 - alescaramanzia@gmail.com