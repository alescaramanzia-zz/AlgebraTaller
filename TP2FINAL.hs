--Trabajo Practico - Parte 2

import Data.Char

--Ejercicio 7--

--Definiciones propias (extendidas de la parte 1)

data Mensaje = TextoClaro Texto | CifradoReverso Mensaje | CifradoCesar Mensaje Desplazamiento | CifradoPalabrasReverso Mensaje 
        deriving (Eq , Show)

type Texto = [Char]
type Desplazamiento = Int

--Funciones auxiliares

esPosibleMensaje :: Texto -> Bool --Verifica que los caracteres ingresados en el string concuerden con los válidos en el TP usando la función elem para filtrar las mayúsculas y el espacio (se crea una lista que contenga todos los caracteres posibles para cotejar).
esPosibleMensaje []                                    = False
esPosibleMensaje [x]    | x `elem` [' ']++['A'..'Z']   = True
esPosibleMensaje (x:xs) | x `elem` [' ']++['A'..'Z']   = esPosibleMensaje xs
                        | otherwise                    = False  

let2int :: Char -> Int --Tomo los valores numéricos (representación ASCII) del caracter ingresado 'c' y calculo su "distancia" respecto de 'A' (para mayúsculas debe estar entre 0 y 25).
let2int c = (ord c - ord 'A')

int2let :: Int -> Char --Convierto el valor de comienzo del alfabeto utilizado ('A') y le sumo n y obtengo el valor numérico del caracter el cuál convierto al caracter.
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


-- Ejercicio 7 - tipo de datos extendido

-- Ejercicio 8--

cifrarCesar :: Mensaje -> Desplazamiento -> Mensaje
cifrarCesar (TextoClaro a) n = CifradoCesar (TextoClaro (cifrarCaracter (a) n)) n

-- EJEMPLO DEL TP

-- *Main> cifrarCesar ( TextoClaro " LA LETRA A Y LA Z " ) 7
-- CifradoCesar (TextoClaro " SH SLAYH H F SH G ") 7
-- *Main> cifrarCesar ( CifradoReverso ( TextoClaro " Z AL Y A ARTEL AL " ) ) 7
-- CifradoReverso (CifradoCesar (TextoClaro " G HS F H HYALS HS ") 7)


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

-- *Main> cifrarCesar (CifradoPalabrasReverso (TextoClaro "NECF SATCAXE ABU")) 6
-- CifradoPalabrasReverso (CifradoCesar (TextoClaro "TKIL YGZIGDK GHA") 6)
-- *Main> cifrarPalabrasReverso (CifradoReverso (CifradoPalabrasReverso (CifradoCesar (TextoClaro "HFIJSFX WJAJWXFW UWZJGF") 5)))
-- CifradoReverso (CifradoPalabrasReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "XFSJIFH WFXWJAJW FGJZWU")) 5))


--Ejercicio 9--

cifrarPalabrasReverso :: Mensaje -> Mensaje
cifrarPalabrasReverso (TextoClaro a)                          = CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  )  )  

-- EJEMPLO DEL TP

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


--Ejercicio 10-- Extensión de las funciones de la parte 1                                                                                                                         

crearMensaje :: Texto -> Mensaje --Suponemos que aún en la parte 2 crearMensaje sólo debe crear un Mensaje en TextoClaro
crearMensaje xs | (esPosibleMensaje xs) == True    = TextoClaro xs
                | otherwise                        = (TextoClaro "ERROR") 

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro a) = (CifradoReverso (TextoClaro (reverse a)))

-- EJEMPLO DEL TP

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
-- *Main> cifrarPalabrasReverso (CifradoReverso (CifradoCesar (TextoClaro "QHLE RGRW DORK") 3))
-- CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "ELHQ WRGR KROD")) 3)


extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro a)                  = a

-- *Main> extraerMensajeParaEnvio (TextoClaro "VIVA EL HASKELL")
-- "VIVA EL HASKELL"

extraerMensajeParaEnvio (CifradoReverso k)              = extraerMensajeParaEnvio k

-- *Main> extraerMensajeParaEnvio (CifradoReverso (TextoClaro "SEROJEM SOL NOS LLEKSAH ED SEROSEFORP SOL"))
-- "SEROJEM SOL NOS LLEKSAH ED SEROSEFORP SOL"

extraerMensajeParaEnvio (CifradoCesar k n) = extraerMensajeParaEnvio k

-- *Main> extraerMensajeParaEnvio ((CifradoCesar (CifradoCesar (TextoClaro "AIX HIXKTMBHGL HKVAXLMKTMBHG") 4) 15))
-- "AIX HIXKTMBHGL HKVAXLMKTMBHG"

extraerMensajeParaEnvio (CifradoPalabrasReverso k) = extraerMensajeParaEnvio k

-- *Main> extraerMensajeParaEnvio (CifradoPalabrasReverso (CifradoPalabrasReverso (CifradoPalabrasReverso (TextoClaro "AVIV LE LLEKSAH"))))
-- "AVIV LE LLEKSAH"
-- *Main> extraerMensajeParaEnvio (CifradoCesar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (CifradoReverso (TextoClaro "LMPFCA PO CLDPN ZAFCR ZSNZ"))) 7)) 4)
-- "LMPFCA PO CLDPN ZAFCR ZSNZ"

descifrar :: Mensaje -> Texto
descifrar (TextoClaro a) = extraerMensajeParaEnvio (TextoClaro a)

-- *Main> descifrar (TextoClaro "ESTO DEBE ANDAR")
-- "ESTO DEBE ANDAR"

descifrar (CifradoReverso k) = reverse (descifrar k)

-- *Main> descifrar (CifradoReverso (CifradoReverso (CifradoReverso (TextoClaro "SELATEM SANUGAL"))))
-- "LAGUNAS METALES"

descifrar (CifradoCesar k n) = cifrarCaracter (descifrar k) (-n) --Revierto el desplazamiento original

-- *Main> descifrar (CifradoCesar (CifradoCesar (TextoClaro "AIX HIXKTMBHGL HKVAXLMKTMBHG") 4) 15)
-- "HPE OPERATIONS ORCHESTRATION"

descifrar (CifradoPalabrasReverso k) = extraerMensajeParaEnvio (cifrarPalabrasReverso (TextoClaro (descifrar k)))  --aplico que si uso cifrarPalabrasReverso sobre un texto CifradoPalabraReverso lo descifro--

-- *Main> descifrar (CifradoPalabrasReverso (TextoClaro "ALOH OGIMA"))
-- "HOLA AMIGO"
-- *Main> descifrar (CifradoCesar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "QRFPVSENQB QR CEHRON")) 4)) 9)
-- "PRUEBA DE DESCIFRADO"

esMensajeCifrado :: Mensaje -> Bool

esMensajeCifrado (TextoClaro a)                    = False
esMensajeCifrado _                                 = True

	-- *Main> esMensajeCifrado (TextoClaro "HOLA COMO ANDAS")
	-- False
	-- *Main> esMensajeCifrado (CifradoPalabrasReverso (TextoClaro "ALOH"))
	-- True
	-- *Main> esMensajeCifrado (CifradoCesar (TextoClaro "JQNC") 2)
	-- True
	-- *Main> esMensajeCifrado (CifradoReverso (TextoClaro "SADNA OMOC ALOH"))
	-- True
	-- *Main> esMensajeCifrado (CifradoReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "HWQDXJD OOHNVDK B VXV VHIRUS")) 3)))
	-- True

esAptoReverso :: Mensaje -> Bool
esAptoReverso (TextoClaro a)             = a /= reverse a -- Revierto  el String que se le pasa como argumento y lo comparo con el original, si son iguales es un palíndromo y no sirve para cifrar.                                                        
esAptoReverso (CifradoReverso k)         = descifrar (CifradoReverso k) /= reverse (descifrar(CifradoReverso k))
esAptoReverso (CifradoCesar k n )        = descifrar (CifradoCesar k n) /= reverse (descifrar(CifradoCesar k n))
esAptoReverso (CifradoPalabrasReverso k) = descifrar (CifradoPalabrasReverso k) /= reverse (descifrar(CifradoPalabrasReverso k))

	-- EJEMPLO DEL TP --
	-- *Main> esAptoReverso ( cifrarReverso ( crearMensaje " ANANA " ) )
	-- False

	-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (cifrarReverso (cifrarReverso (CifradoReverso (CifradoReverso (TextoClaro "ATALEDEMONIACOCAINOMEDELATA")))))))
	-- False
	-- *Main> esAptoReverso (cifrarPalabrasReverso (CifradoPalabrasReverso (CifradoPalabrasReverso (CifradoPalabrasReverso (cifrarPalabrasReverso (CifradoPalabrasReverso (TextoClaro "VIVA HASKELL")))))))
	-- True
	-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (cifrarReverso (cifrarReverso (CifradoReverso (CifradoReverso (TextoClaro "HOLA AMIGO MIO")))))))
	-- True
	-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (cifrarReverso (cifrarReverso (CifradoReverso (CifradoReverso (TextoClaro "ANITALAVALATINA")))))))
	-- False
	-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "HWQDXJD OOHNVDK B VXV VHIRUS")) 3))))
	-- True
	-- *Main> esAptoReverso (cifrarReverso (CifradoReverso (cifrarReverso (cifrarReverso (CifradoReverso (CifradoReverso (TextoClaro "ATALE DEMONIACO CAIN O ME DELATA")))))))
	-- True

-- Bibliografía utilizada

-- http://www.glc.us.es/~jalonso/vestigium/i1m2012-el-cifrado-cesar-en-haskell/ (buscamos comparar estructura de nuestra función de cifradoCesar con otras)
-- http://www.haskell.org/hoogle/ (para buscar expresiones y funciones en detalle).
-- Diapositivas del taller
-- http://aprendehaskell.es/
-- https://ronnyml.wordpress.com/2007/12/08/funciones-utiles-en-listas-haskell/

-- Miraglia, Jorge - 596/13 - Jorge10192@live.com.ar
-- Arriondo, Alexis - 234/17 - alescaramanzia@gmail.com