--Trabajo Practico - Parte 2

import Data.Char

--Definiciones propias

data Mensaje = TextoClaro Texto | CifradoReverso Mensaje | CifradoCesar Mensaje Desplazamiento | CifradoPalabrasReverso Mensaje 
        deriving (Eq , Show)

type Texto = [Char]
type Desplazamiento = Int

--Funciones auxiliares

let2int :: Char -> Int --Tomo los valores numéricos (representación ASCII) del caracter ingresado 'c' y calculo su "distancia" respecto de 'A' (para mayúsculas debe estar entre 0 y 25).
let2int c = (ord c - ord 'A')

int2let :: Int -> Char --Convierto el valor de comienzo del alfabeto utilizado ('A') y le sumo n (0<n<25) y obtengo el valor numérico del caracter el cuál convierto al caracter.
int2let n = chr (ord 'A' + n)

desplaza :: Int -> Char -> Char --Dentro del rango ['A'..'Z'] dado un desplazamiento n reemplaza el caracter c por el mismo desplazado n posiciones, si c está fuera del rango devuelve c.
desplaza n c 
    | elem c ['A'..'Z'] = int2let ((let2int c + n) `mod` 26)
    | otherwise         = c

cifrarCaracter :: Texto -> Desplazamiento -> Texto --Cifra un string xs pasado como parámetro con un desplazamiento de n posiciones, la función desplaza limita el cifrado sólo a las mayúsculas.
cifrarCaracter xs n = [desplaza n x | x <- xs]

listaTexto :: Texto -> [Texto] --Usa la función words de Prelude para dividir un String delimitado por espacios en una lista que contenga como elementos cada palabra del string.
listaTexto a = words a

reversoListaTexto :: [Texto] -> [Texto] --Invierte cada uno de los elementos de la lista de strings pasada como argumento empezando por el primer elemento y con los siguientes de forma recursiva.
reversoListaTexto [] = []
reversoListaTexto xs = [reverse (head xs)] ++ reversoListaTexto (tail xs)

textoLista :: [Texto] -> Texto --Usa la función unwords de Prelude para devolver un string formado por los elementos de una lista de strings agregando los espacios entre palabras.
textoLista xs = unwords xs

--Ejercicio 7--

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro a) = (CifradoReverso (TextoClaro (reverse a)))

-- EJEMPLO DE TP
-- *Main> cifrarReverso ( cifrarReverso ( TextoClaro " SIEMPRE REVERSO " ) )
-- CifradoReverso (CifradoReverso (TextoClaro " SIEMPRE REVERSO "))


cifrarReverso (CifradoReverso k) = CifradoReverso (cifrarReverso k)

-- Dos ejemplos: cuando la función se aplica un número par de veces devuelve el texto normal y si es impar invertido.
-- *Main> cifrarReverso (cifrarReverso (cifrarReverso (cifrarReverso (cifrarReverso (TextoClaro "PRUEBA UNO")))))
-- CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (TextoClaro "ONU ABEURP")))))
-- *Main> cifrarReverso (cifrarReverso (cifrarReverso (cifrarReverso (cifrarReverso (cifrarReverso (TextoClaro "PRUEBA UNO"))))))
-- CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (CifradoReverso (TextoClaro "PRUEBA UNO"))))))
-- *Main> 

cifrarReverso (CifradoCesar k n) = CifradoCesar (cifrarReverso k) n

-- *Main> cifrarReverso (CifradoCesar (TextoClaro "LSPE XSHS FMIR") 4) 
-- CifradoCesar (CifradoReverso (TextoClaro "RIMF SHSX EPSL")) 4
-- *Main> cifrarReverso (CifradoCesar (CifradoReverso (TextoClaro "RIMF SHSX EPSL")) 4)
-- CifradoCesar (CifradoReverso (CifradoReverso (TextoClaro "LSPE XSHS FMIR"))) 4
-- *Main> 

cifrarReverso (CifradoPalabrasReverso k) = CifradoPalabrasReverso (cifrarReverso k)

-- *Main> cifrarReverso (CifradoPalabrasReverso (TextoClaro "LINEA C"))
-- CifradoPalabrasReverso (CifradoReverso (TextoClaro "C AENIL"))

-- *Main> cifrarReverso (cifrarCesar (CifradoPalabrasReverso (CifradoReverso (TextoClaro "HOLA TODO BIEN"))) 3)
-- CifradoPalabrasReverso (CifradoReverso (CifradoCesar (CifradoReverso (TextoClaro "QHLE RGRW DORK")) 3))





--Ejercicio 8--

cifrarCesar :: Mensaje -> Desplazamiento -> Mensaje
cifrarCesar (TextoClaro a) n = CifradoCesar (TextoClaro (cifrarCaracter (a) n)) n

-- EJEMPLO DEL TP

-- *Main> cifrarCesar ( TextoClaro " LA LETRA A Y LA Z " ) 7
-- CifradoCesar (TextoClaro " SH SLAYH H F SH G ") 7

cifrarCesar (CifradoReverso k) n = CifradoReverso (cifrarCesar k n)

-- *Main> cifrarCesar (CifradoReverso (TextoClaro "QHLE RGRW DORK")) 4
-- CifradoReverso (CifradoCesar (TextoClaro "ULPI VKVA HSVO") 4)
-- *Main> cifrarCesar (CifradoReverso (TextoClaro "QHLE RGRW DORK")) (-6) -- Desplazamiento negativo
-- CifradoReverso (CifradoCesar (TextoClaro "KBFY LALQ XILE") (-6))
-- *Main> cifrarCesar (CifradoReverso (TextoClaro "QHLE RGRW DORK")) 0 -- Desplazamiento nulo
-- CifradoReverso (CifradoCesar (TextoClaro "QHLE RGRW DORK") 0)

cifrarCesar (CifradoCesar k m) n = CifradoCesar  (cifrarCesar k n) m

-- *Main> cifrarCesar (CifradoCesar (TextoClaro " SH SLAYH H F SH G ") 7) 5
-- CifradoCesar (CifradoCesar (TextoClaro " XM XQFDM M K XM L ") 5) 7

cifrarCesar (CifradoPalabrasReverso k) n = CifradoPalabrasReverso (cifrarCesar k n)

-- *Main> cifrarCesar (CifradoPalabrasReverso (CifradoReverso (TextoClaro "HOLA TODO BIEN"))) 3
-- CifradoPalabrasReverso (CifradoReverso (CifradoCesar (TextoClaro "KROD WRGR ELHQ") 3))

cifrarPalabrasReverso :: Mensaje -> Mensaje
cifrarPalabrasReverso (TextoClaro a)                          = CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  )  )  

-- *Main> cifrarPalabrasReverso (TextoClaro "LINEA C RETIRO CONSTITUCION")
-- CifradoPalabrasReverso (TextoClaro "AENIL C ORITER NOICUTITSNOC")

cifrarPalabrasReverso (CifradoPalabrasReverso k)              = CifradoPalabrasReverso (cifrarPalabrasReverso k)

-- *Main> cifrarPalabrasReverso (CifradoPalabrasReverso (TextoClaro "AENIL C ORITER NOICUTITSNOC"))
-- CifradoPalabrasReverso (CifradoPalabrasReverso (TextoClaro "LINEA C RETIRO CONSTITUCION"))

cifrarPalabrasReverso (CifradoReverso k)                      = CifradoReverso (cifrarPalabrasReverso k)

-- *Main> cifrarPalabrasReverso (CifradoReverso (TextoClaro "QHLE RGRW DORK"))
-- CifradoReverso (CifradoPalabrasReverso (TextoClaro "ELHQ WRGR KROD"))

cifrarPalabrasReverso (CifradoCesar k m)                      = CifradoCesar (cifrarPalabrasReverso k) m

-- *Main> cifrarPalabrasReverso (CifradoCesar (TextoClaro "QHLE RGRW DORK") 0)
-- CifradoCesar (CifradoPalabrasReverso (TextoClaro "ELHQ WRGR KROD")) 0

-- *Main> cifrarPalabrasReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "ELHQ WRGR KROD")) 0)
-- CifradoCesar (CifradoPalabrasReverso (CifradoPalabrasReverso (TextoClaro "QHLE RGRW DORK"))) 0


--ejercicio 10-- --Extiendo funciones que te piden en la parte 2 de la parte 1-- --crearMensaje no lo extiendo pues se valido solo para Texto--

esPosibleMensaje :: Texto -> Bool --Verifica que los caracteres ingresados en el string concuerden con los válidos en el TP usando la función elem para filtrar las mayúsculas y el espacio (se crea una lista que contenga todos los caracteres posibles para cotejar).
esPosibleMensaje []                                    = False
esPosibleMensaje [x]    | x `elem` [' ']++['A'..'Z']   = True
esPosibleMensaje (x:xs) | x `elem` [' ']++['A'..'Z']   = esPosibleMensaje xs
                        | otherwise                    = False                                                                                                                                 

crearMensaje :: Texto -> Mensaje
crearMensaje xs | (esPosibleMensaje xs) == True    = TextoClaro xs
                | otherwise                        = (TextoClaro "ERROR") 


descifrar :: Mensaje -> Texto
descifrar (TextoClaro a) = a
descifrar (CifradoReverso (TextoClaro a)) = reverse a 
descifrar (CifradoCesar (TextoClaro a) n) = (cifrarCaracter a (n*(-1)))
descifrar (CifradoPalabrasReverso (TextoClaro a)) = (textoLista (reversoListaTexto (listaTexto a)))

descifrar (CifradoReverso k) = descifrar (CifradoReverso (descifrar k))

-- descifrar (CifradoReverso k) = CifradoReverso descifrar
-- descifrar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) = (reverse (textoLista (reversoListaTexto (listaTexto a))))
-- descifrar (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) = (cifrarCaracter (textoLista (reversoListaTexto (listaTexto a))) (n*(-1)) )
--aplico las 3 operaciones--
-- descifrar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = (reverse (cifrarCaracter (textoLista (reversoListaTexto (listaTexto a))) (n*(-1)) ) )

--extender

-- extraerMensajeParaEnvio :: Mensaje -> Texto
-- extraerMensajeParaEnvio (TextoClaro a)                  = (reverse a)
-- extraerMensajeParaEnvio (CifradoReverso (TextoClaro a)) = a
-- extraerMensajeParaEnvio (CifradoCesar (TextoClaro a) n) = a
-- extraerMensajeParaEnvio (CifradoPalabrasReverso (TextoClaro a)) = a
-- extraerMensajeParaEnvio (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) = a
-- extraerMensajeParaEnvio (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = a
-- extraerMensajeParaEnvio (CifradoCesar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) n ) = a
-- --extraerMensajeParaEnvio (CifradoCesar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) n ) = a

--  extraerMensajeParaEnvio :: Mensaje -> Texto
--  extraerMensajeParaEnvio (TextoClaro a)                  = reverse a
--  extraerMensajeParaEnvio _ = a

esMensajeCifrado :: Mensaje -> Bool

esMensajeCifrado (CifradoReverso (TextoClaro a)) = True 
esMensajeCifrado (CifradoCesar (TextoClaro a) n) = True
esMensajeCifrado (CifradoPalabrasReverso (TextoClaro a)) = True
esMensajeCifrado (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) = True
esMensajeCifrado (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = True
esMensajeCifrado (CifradoCesar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) n ) = True
esMensajeCifrado (TextoClaro a)                  = False

-- esAptoReverso :: Mensaje -> Bool
-- esAptoReverso (TextoClaro a)                  | ((a) == (reverse a))                = False
--                                               | otherwise                           = True
-- esAptoReverso (CifradoReverso (TextoClaro a)) | ((a) == (reverse a))                = False
--                                               | otherwise    

--Ejemplos de ejecución--

-- [I]  ~/g/AlgebraTaller   master ±  ghci                                                                                                                           I 
-- GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
-- Loading package ghc-prim ... linking ... done.
-- Loading package integer-gmp ... linking ... done.
-- Loading package base ... linking ... done.
-- Prelude> :l TP2
-- [1 of 1] Compiling Main             ( TP2.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main>  cifrarCesar (TextoClaro "OPERATIONS ORCHESTRATION") 2
-- CifradoCesar (TextoClaro "QRGTCVKQPU QTEJGUVTCVKQP") 2
-- *Main> cifrarPalabrasReverso (CifradoCesar (TextoClaro "QRGTCVKQPU QTEJGUVTCVKQP") 2)
-- CifradoCesar (CifradoPalabrasReverso (TextoClaro "UPQKVCTGRQ PQKVCTVUGJETQ")) 2
-- *Main> descifrar (CifradoCesar (CifradoPalabrasReverso (TextoClaro "UPQKVCTGRQ PQKVCTVUGJETQ")) 2)
-- "OPERATIONS ORCHESTRATION"
-- *Main> 

--Bibliografía utilizada

--http://www.haskell.org/hoogle/ (para buscar expresiones y funciones en detalle).
--Diapositivas del taller
--http://aprendehaskell.es/
--
--

-- Miraglia, Jorge - 666/17 - Jorge10192@live.com.ar
-- Arriondo, Alexis - 234/17 - alescaramanzia@gmail.com