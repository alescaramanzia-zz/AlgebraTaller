{-Trabajo Practico-} -- anda todo bien salvo un detalle en el ejercicio 1 --

data Mensaje = TextoClaro Texto | CifradoReverso Mensaje  deriving (Eq , Show)
type Texto = [Char]
--ejercicio 1 bis-- --Funcion auxiliar esPosibleMensaje--
esPosibleMensaje :: Texto -> Bool --Cuando Hago esPosibleMensaje "CaracterValido , CaracterNoValido, ...." Toma al 2do caracter como Valido , Si pongo"Caracter No Valido"en cualquier otro lado o mas de 1 Caracter no valido anda bien--
--esPosibelMensaje "CaracterValido,CaracterNoValido" anda bien, solo "CaracterNoValido en la 2da posicion no da error puse posibles formas de matarlo ninguna me anduvo"

esPosibleMensaje []   = False

esPosibleMensaje [x]  | (x `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ']) = True
                      | otherwise                                                                                                                    = False

esPosibleMensaje [x,y] | (x `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ']) && (y `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' '])  = True
                       | otherwise                                                                                                                                                                                                                                                     = False

esPosibleMensaje (x:xs) | ((length xs) == 1)                                                                                                                                 = esPosibleMensaje [x]
 --Dos Posible Formas de solucionarlo--                      -- | ((head (tail xs)) `notElem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' '])     = False --
 --Ninguna anduvo pero ambos corrian bien el programa--      -- | ((esPosibleMensaje [(head xs),(head (tail xs))]) == False)                                                                                         = False --
                        | (x `elem` ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z',' '])                       = esPosibleMensaje ((head (tail xs)):tail xs) 
                        | otherwise                                                                                                                                          = False

--ejercicio 1--
crearMensaje :: Texto -> Mensaje
crearMensaje xs | (esPosibleMensaje xs) == True    = TextoClaro xs
                | otherwise                        = (TextoClaro ['E','R','R','O','R']) 
--del ejercicio 1 llama solo al error si el 1er elemento es distinto de volcal o espacio, para los demas elementos no anda!, los toma como posibles valores de texto--
--Ejercicio 2--

esMensajeCifrado :: Mensaje -> Bool
esMensajeCifrado (TextoClaro a) = False
esMensajeCifrado (CifradoReverso (TextoClaro a)) = True 

--ejercicio 3--

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro a) = CifradoReverso (TextoClaro (reverse a))
cifrarReverso (CifradoReverso (TextoClaro a)) = CifradoReverso (TextoClaro (reverse a))

--ejercicio 4--
extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro a) = (reverse a)
extraerMensajeParaEnvio (CifradoReverso (TextoClaro a)) = a

--ejercicio 5--
descifrar :: Mensaje -> Texto
descifrar (TextoClaro a) = a
descifrar (CifradoReverso (TextoClaro a)) = (reverse a)

--ejercicio 6-- 
esAptoReverso :: Mensaje -> Bool
esAptoReverso (TextoClaro a) | ((a) == (reverse a))                                 = False
                             | otherwise                                            = True
esAptoReverso (CifradoReverso (TextoClaro a)) | ((a) == (reverse a))                                 = False
                                              | otherwise                                            = True

