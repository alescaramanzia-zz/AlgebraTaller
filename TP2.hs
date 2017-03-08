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

--ejercicio 7-- --(No se a que se refiere) (Supuse que era cargar lo del ejercicio 1 aca, no se si hay que cargar las demas tambien)--

cifrarReverso :: Mensaje -> Mensaje
--Pertenece a la parte 1--
cifrarReverso (TextoClaro a)                  = CifradoReverso (TextoClaro (reverse a))
cifrarReverso (CifradoReverso (TextoClaro a)) = CifradoReverso (TextoClaro (reverse a))

--extiendo ejercicio 7 para hacer reverso de cifradoCesar y cifradoPalabrasReverso--
--CifradoCesar solo---
cifrarReverso (CifradoCesar (TextoClaro a) n)                  = CifradoReverso (CifradoCesar (TextoClaro (reverse a)) n)
cifrarReverso (CifradoReverso (CifradoCesar (TextoClaro a) n)) = CifradoReverso (CifradoCesar (TextoClaro (reverse a)) n)

--CifradoPalabrasReverso solo--
cifrarReverso (CifradoPalabrasReverso (TextoClaro a))                  = CifradoReverso (CifradoPalabrasReverso (TextoClaro (reverse a)))
cifrarReverso (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) = CifradoReverso (CifradoPalabrasReverso (TextoClaro (reverse a)))

--extiendo ejercicio 7 cifradoCesar y cifradoPalabrasReverso ambas juntas--
cifrarReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)                  = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (reverse a))) n)
cifrarReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (reverse a))) n)


--Ejercicio 8--

cifrarCesar :: Mensaje -> Desplazamiento -> Mensaje
--TextoClaro--
cifrarCesar (TextoClaro a) n                  = CifradoCesar (TextoClaro (cifrarCaracter (a) n)) n
cifrarCesar (CifradoCesar (TextoClaro a) m) n = CifradoCesar (TextoClaro (cifrarCaracter (a) m)) (n+m) --La suma (n+m) indica el desplazamiento total respecto a la frase sin cifrar.
--CifradoReverso--
cifrarCesar (CifradoReverso (TextoClaro a)) n                  = CifradoReverso (cifrarCesar (TextoClaro a) n)
cifrarCesar (CifradoReverso (CifradoCesar (TextoClaro a) n)) m = CifradoReverso (CifradoCesar (TextoClaro (cifrarCaracter (a) m)) (m+n))

--Ejercicio 8 ampliado sobre cifrarPalabrasReverso y cifrarReverso - cifrarPalabrasReverso.

--CifrarPalabrasReverso--
cifrarCesar (CifradoPalabrasReverso (TextoClaro a)) n                  = CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) n) )) n
cifrarCesar (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) m = CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) (m)))) (n+m)

--CifrarPalabrasReverso con CifradoReverso--
cifrarCesar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) n                  = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) n))) n)
cifrarCesar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) m = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) (m)))) (n+m))

--ejercicio 9--

cifrarPalabrasReverso :: Mensaje -> Mensaje
--textoClaro--
cifrarPalabrasReverso (TextoClaro a)                          = CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  )  )  
cifrarPalabrasReverso (CifradoPalabrasReverso (TextoClaro a)) = CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  ))

--CifradoReverso solo--
cifrarPalabrasReverso (CifradoReverso (TextoClaro a))                            = CifradoReverso (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  ) ))
cifrarPalabrasReverso (CifradoReverso (CifradoPalabrasReverso (TextoClaro a) ) ) = CifradoReverso (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  ) ))

--CifradoCesar solo--
cifrarPalabrasReverso (CifradoCesar (TextoClaro a) m)                          = CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a))))) m
cifrarPalabrasReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) m) = CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (reversoListaTexto (listaTexto a))))) m

--CifradoReverso con CifradoCesar--
cifrarPalabrasReverso (CifradoReverso (CifradoCesar (TextoClaro a) n))                          = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (reversoListaTexto (listaTexto a))) )) n)
cifrarPalabrasReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (reversoListaTexto (listaTexto a))) )) n)

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
descifrar (CifradoReverso (TextoClaro a)) = (reverse a)

--extiendo descifrar para poder descifrar cualquier mensaje que quiera de acuerdo a las nuevas funciones que tengo--
--aplico 1 operacion sola-- 
descifrar (CifradoCesar (TextoClaro a) n) = (cifrarCaracter a (n*(-1)))
descifrar (CifradoPalabrasReverso (TextoClaro a)) = (textoLista (reversoListaTexto (listaTexto a)))
--aplico 2 operaciones a la vez--
descifrar (CifradoReverso (CifradoCesar (TextoClaro a) n)) = (reverse (cifrarCaracter a (n*(-1))))
descifrar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) = (reverse (textoLista (reversoListaTexto (listaTexto a))))
descifrar (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) = (cifrarCaracter (textoLista (reversoListaTexto (listaTexto a))) (n*(-1)) )
--aplico las 3 operaciones--
descifrar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = (reverse (cifrarCaracter (textoLista (reversoListaTexto (listaTexto a))) (n*(-1)) ) )

--extender
-- esAptoReverso :: Mensaje -> Bool
-- extraerMensajeParaEnvio :: Mensaje -> Texto
-- esMensajeCifrado :: Mensaje -> Bool

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