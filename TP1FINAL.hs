--Trabajo Practico - Parte 1

--Definiciones propias

data Mensaje = TextoClaro Texto | CifradoReverso Mensaje  
        deriving (Eq , Show)

type Texto = [Char]

--Funciones auxiliares

esPosibleMensaje :: Texto -> Bool --Verifica que los caracteres ingresados en el string concuerden con los válidos en el TP usando la función elem para filtrar las mayúsculas y el espacio (se crea una lista que contenga todos los caracteres posibles para cotejar).
esPosibleMensaje []                                    = False
esPosibleMensaje [x]    | x `elem` [' ']++['A'..'Z']   = True
esPosibleMensaje (x:xs) | x `elem` [' ']++['A'..'Z']   = esPosibleMensaje xs
                        | otherwise                    = False
--Ejercicio 1--

crearMensaje :: Texto -> Mensaje
crearMensaje xs | (esPosibleMensaje xs) == True = TextoClaro xs
                | otherwise                     = (TextoClaro "ERROR") 

-- *Main> crearMensaje ("AGUANTE HASKELLL")
-- TextoClaro "AGUANTE HASKELLL"
-- *Main> crearMensaje ("Aguante Haskelll")
-- TextoClaro "ERROR"
-- *Main> crearMensaje ("    ")
-- TextoClaro "    "
-- *Main> crearMensaje ("1234")
-- TextoClaro "ERROR"
-- *Main> crearMensaje " -PRIMERA PRUEBA"
-- TextoClaro "ERROR"
-- *Main> crearMensaje "92PRIMERA PRUEBA"
-- TextoClaro "ERROR"

--Ejercicio 2--

esMensajeCifrado :: Mensaje -> Bool
esMensajeCifrado (TextoClaro a)                  = False
esMensajeCifrado _                               = True 

-- *Main> esMensajeCifrado (TextoClaro "HOLA AMIGO MIO")
-- False
-- *Main> esMensajeCifrado (cifrarReverso (TextoClaro "HOLA AMIGO MIO"))
-- True


--Ejercicio 3--

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro a)                  = CifradoReverso (TextoClaro (reverse a))
cifrarReverso (CifradoReverso k)              = CifradoReverso (cifrarReverso k)

--EJEMPLOS DEL TP
-- *Main> cifrarReverso ( TextoClaro " SIEMPRE REVERSO ")
-- CifradoReverso (TextoClaro " OSREVER ERPMEIS ")
-- *Main> cifrarReverso ( cifrarReverso ( TextoClaro " SIEMPRE REVERSO "))
-- CifradoReverso (CifradoReverso (TextoClaro " SIEMPRE REVERSO "))

-- *Main> cifrarReverso (CifradoReverso (cifrarReverso (CifradoReverso (TextoClaro "AGUANTE HASKELL"))))
-- CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (TextoClaro "AGUANTE HASKELL"))))

--Ejercicio 4--

extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro a)                  = a
extraerMensajeParaEnvio (CifradoReverso k)              = extraerMensajeParaEnvio k

--EJEMPLO DEL TP
-- *Main> extraerMensajeParaEnvio (CifradoReverso (TextoClaro " OSREVER ERPMEIS "))
-- " OSREVER ERPMEIS "

-- *Main> extraerMensajeParaEnvio (TextoClaro "HASKELL MANDAAAA")
-- "HASKELL MANDAAAA"
-- *Main> extraerMensajeParaEnvio (cifrarReverso (CifradoReverso (cifrarReverso (CifradoReverso (cifrarReverso (TextoClaro "HASKELL MANDAAAA"))))))
-- "AAAADNAM LLEKSAH"


--Ejercicio 5--

descifrar :: Mensaje -> Texto
descifrar (TextoClaro a)                  = a
descifrar (CifradoReverso k)              = (reverse (descifrar k))

-- EJEMPLO DEL TP
-- *Main> descifrar ( CifradoReverso ( TextoClaro " OSREVER ERPMEIS "))
-- " SIEMPRE REVERSO "

-- *Main> descifrar (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (TextoClaro "AAAADNAM LLEKSAH"))))))
-- "HASKELL MANDAAAA"
-- *Main> descifrar (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (TextoClaro "OIM OGIMA ALOH"))))))
-- "HOLA AMIGO MIO"
-- *Main> descifrar (cifrarReverso (cifrarReverso (cifrarReverso (cifrarReverso (TextoClaro "HOLA AMIGO MIO")))))
-- "HOLA AMIGO MIO"

--Ejercicio 6-- 

esAptoReverso :: Mensaje -> Bool
esAptoReverso (TextoClaro a)             = a /= reverse a                                                        
esAptoReverso (CifradoReverso k)         = descifrar (CifradoReverso k) /= reverse (descifrar(CifradoReverso k))

-- EJEMPLO DEL TP --
-- *Main > esAptoReverso ( TextoClaro "NEUQUEN")
-- False
-- *Main > esAptoReverso ( cifrarReverso ( crearMensaje "ANANA"))
-- False
-- *Main > esAptoReverso ( cifrarReverso ( crearMensaje "NEUQUEN"))
-- False
-- *Main > esAptoReverso ( cifrarReverso ( cifrarReverso ( crearMensaje "OJOTA")))
-- True


-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (cifrarReverso (cifrarReverso (CifradoReverso (CifradoReverso (TextoClaro "ATALEDEMONIACOCAINOMEDELATA")))))))
-- False
-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (cifrarReverso (cifrarReverso (CifradoReverso (CifradoReverso (TextoClaro "ATALE DEMONIACO CAIN O ME DELATA")))))))
-- True


--Bibliografía utilizada

--http://www.haskell.org/hoogle/ (para buscar expresiones y funciones en detalle).
--Diapositivas del taller
--http://aprendehaskell.es/
--https://ronnyml.wordpress.com/2007/12/08/funciones-utiles-en-listas-haskell/

-- Miraglia, Jorge - 596/13 - Jorge10192@live.com.ar
-- Arriondo, Alexis - 234/17 - alescaramanzia@gmail.com