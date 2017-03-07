{-Trabajo Practico Parte 2-}
import Data.Char
data Mensaje = TextoClaro Texto | CifradoReverso Mensaje | CifradoCesar Mensaje Desplazamiento | CifradoPalabrasReverso Mensaje deriving (Eq , Show )

type Texto = [Char]
type Desplazamiento = Int


--ejercicio 7-- --(No se a que se refiere) (Supuse que era cargar lo del ejercicio 1 aca, no se si hay que cargar las demas tambien)--

cifrarReverso :: Mensaje -> Mensaje
--Pertenece a la parte 1--
cifrarReverso (TextoClaro a) = (CifradoReverso (TextoClaro (reverse a)))
cifrarReverso (CifradoReverso (TextoClaro a)) = CifradoReverso (TextoClaro (reverse a))

--extiendo ejercicio 7 para hacer reverso de cifradoCesar y cifradoPalabrasReversa--
--CifradoCeasr solo---
cifrarReverso (CifradoCesar (TextoClaro a) n) = CifradoReverso (CifradoCesar (TextoClaro (reverse a)) n)
cifrarReverso (CifradoReverso (CifradoCesar (TextoClaro a) n)) = CifradoReverso (CifradoCesar (TextoClaro (reverse a)) n)

--CifradoPalabrasReverso solo--
cifrarReverso (CifradoPalabrasReverso (TextoClaro a)) = CifradoReverso (CifradoPalabrasReverso (TextoClaro (reverse a)))
cifrarReverso (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) = CifradoReverso (CifradoPalabrasReverso (TextoClaro (reverse a)))

--extiendo ejercicio 7 cifradoCesar y cifradoPalabrasReverso ambas juntas--
cifrarReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (reverse a))) n)
cifrarReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (reverse a))) n)


--ejercicio 8 bis -- --Primero hago la sugerencia --
let2int :: Char -> Int
let2int c = (ord c - ord 'A')

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

desplaza :: Int -> Char -> Char
desplaza n c 
    | elem c ['A'..'Z'] = int2let ((let2int c + n) `mod` 26)
    | otherwise         = c

cifrarCaracter :: Texto -> Desplazamiento -> Texto
cifrarCaracter xs n = [desplaza n x | x <- xs]

--ejercicio 8--  --cifrarCesar la programe para que se utilize en el orden (CifradoReverso -> CifradoCesar -> CifradoPalabrasRverso -> TextoClaro) como muestra el ejemplo del tp --
cifrarCesar :: Mensaje -> Desplazamiento -> Mensaje
--TextoClaro--
cifrarCesar (TextoClaro a) n = (CifradoCesar (TextoClaro (cifrarCaracter (a) n)) n)
cifrarCesar (CifradoCesar (TextoClaro a) m) n = (CifradoCesar (TextoClaro (cifrarCaracter (a) (m))) (n+m))
--CifradoReverso--
cifrarCesar (CifradoReverso (TextoClaro a)) n = (CifradoReverso (cifrarCesar (TextoClaro a) n))
cifrarCesar (CifradoReverso (CifradoCesar (TextoClaro a) n)) m = (CifradoReverso (CifradoCesar (TextoClaro (cifrarCaracter (a) m)) (m+n)))

--ejercicio 8 ampliado sobre cifrarPalabrasReverso y cifrarReverso combinado cifrarPalabrasReverso--

--CifrarPalabrasReverso--
cifrarCesar (CifradoPalabrasReverso (TextoClaro a)) n = CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) n) )) n
cifrarCesar (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n) m = CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) (m)))) (n+m)

--CifrarPalabrasReverso con CifradoReverso--
cifrarCesar (CifradoReverso (CifradoPalabrasReverso (TextoClaro a))) n = (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) n))) n))
cifrarCesar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) m = (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (cifrarCaracter (a) (m)))) (n+m)))


--ejercicio 9 bis-- --Funciones auxiliares y operaciones de haskell words, unwords--
listaTexto :: Texto -> [Texto]
listaTexto a = words a

reversoListaTexto :: [Texto] -> [Texto]
reversoListaTexto [] = []
reversoListaTexto xs = [(reverse (head xs))] ++ reversoListaTexto (tail xs)

textoLista :: [Texto] -> Texto
textoLista xs = unwords xs

--ejercicio 9--
cifrarPalabrasReverso :: Mensaje -> Mensaje
--textoClaro--
cifrarPalabrasReverso (TextoClaro a) = (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  )  )  )
cifrarPalabrasReverso (CifradoPalabrasReverso (TextoClaro a)) = (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  )))

--CifradoReverso solo--
cifrarPalabrasReverso (CifradoReverso (TextoClaro a)) = (CifradoReverso (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  ) )))
cifrarPalabrasReverso (CifradoReverso (CifradoPalabrasReverso (TextoClaro a) ) ) = CifradoReverso (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a)  )  ) ))

--CifradoCesar solo--
cifrarPalabrasReverso (CifradoCesar (TextoClaro a) m) = CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (  reversoListaTexto (listaTexto a))))) m
cifrarPalabrasReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) m) = CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (reversoListaTexto (listaTexto a))))) m

--CifradoReverso con CifradoCesar--
cifrarPalabrasReverso (CifradoReverso (CifradoCesar (TextoClaro a) n)) = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (reversoListaTexto (listaTexto a))) )) n)
cifrarPalabrasReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro a)) n)) = CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro (textoLista (reversoListaTexto (listaTexto a))) )) n)

--ejercicio 10-- --Extiendo funciones que te piden en la parte 2 de la parte 1-- --crearMensaje no lo extiendo pues se valido solo para Texto--
esPosibleMensaje :: Texto -> Bool  --Cuando Hago esPosibleMensaje "CaracterValido , CaracterNoValido, ...." Toma al 2do caracter como Valido , Si pongo"Caracter No Valido"en cualquier otro lado o mas de 1 Caracter no valido anda bien--
--esPosibelMensaje "CaracterValido,CaracterNoValido" anda bien, solo "CaracterNoValido en la 2da posicion no da error puse posibles formas de matarlo ninguna me anduvo"
esPosibleMensaje []   = False

esPosibleMensaje [x]  | (x `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ']) = True
                      | otherwise                                                                                                                    = False

esPosibleMensaje [x,y] | (x `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ']) && (y `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' '])  = True
                       | otherwise                                                                                                                                                                                                                                                     = False

esPosibleMensaje (x:xs) | ((length xs) == 1)                                                                                                                                = esPosibleMensaje [x]
--Dos Posible Formas de solucionarlo--                      -- | ((head (tail xs)) `notElem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' '])     = False --
--Ninguna anduvo pero ambos corrian bien el programa--      -- | ((esPosibleMensaje [(head xs),(head (tail xs))]) == False)                                                                                         = False --                        
                        | (x `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' '])                       = esPosibleMensaje ((head (tail xs)):tail xs) 
                        | otherwise                                                                                                                                          = False

crearMensaje :: Texto -> Mensaje
crearMensaje xs | (esPosibleMensaje xs) == True    = TextoClaro xs
                | otherwise                        = (TextoClaro ['E','R','R','O','R']) 


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