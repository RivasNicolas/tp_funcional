module Solucion where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: Cambalache
-- Integrante 1: Ezequiel Juan Fernández, ezequieljuanfernandez2003@gmail.com, 774/23
-- Integrante 2: Tomas Benjamin Ramirez, rtb.fcen@gmail.com, 530/23
-- Integrante 3: Nicolás Rivas, nicolas@rivasgarcia.com.ar, 321/23
-- Integrante 4: Juan Ignacio Villalba, juaninator360@gmail.com, 759/23

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Devuelve un [String] cuyos elementos son los nombres de cada usuario de la red
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = eliminarRepetidos (proyectarNombres (usuarios rs))

-- proyectarNombres: Toma una lista de Usuarios y devuelve
-- una lista de String donde cada elemento es el nombre 
-- de usuario de cada uno de los elementos de la lista de
-- Usuarios.
proyectarNombres :: [Usuario] -> [String]
proyectarNombres []     = [] 
proyectarNombres [u]    = [nombreDeUsuario u]
proyectarNombres (u:us) = [nombreDeUsuario u] ++ proyectarNombres us

-- eliminarRepetidos: Toma una lista de algun tipo
-- y la devuelve pero cada elemento aparece una única vez.
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos []  = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs)
    | not (tieneRepetidos (x:xs)) = (x:xs)
    | pertenece x xs              = eliminarRepetidos xs
    | otherwise                   = x:eliminarRepetidos xs
    
 -- tieneRepetidos: Verifica si una lista tiene una elemento
-- mas de una véz
tieneRepetidos :: (Eq t) => [t] -> Bool
tieneRepetidos [] = False
tieneRepetidos [x] = False
tieneRepetidos (x:xs)
    | pertenece x xs  = True
    | otherwise       = tieneRepetidos xs
    
 -- pertenece: Verifica si un elemento pertenece
-- a una lista de elementos del mismo tipo. 
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x [y] = x == y
pertenece x (y:ys)
    | x == y    = True
    | otherwise = pertenece x ys

-- Devuelve una lista de usuarios que estan relacionados con un usuario especifico
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rsRed uUsuario = amigosDe' (relaciones rsRed) uUsuario

amigosDe' :: [Relacion] -> Usuario -> [Usuario]
amigosDe' [] _ = []
amigosDe' [(u1,u2)] uUsuario
    | uUsuario == u1 = [u2]
    | uUsuario == u2 = [u1]
    | otherwise      = []
amigosDe' ((u1,u2):rRelaciones) uUsuario
    | uUsuario == u1 = u2:amigosDe' rRelaciones uUsuario
    | uUsuario == u2 = u1:amigosDe' rRelaciones uUsuario
    | otherwise      = amigosDe' rRelaciones uUsuario

-- Devuelve un entero que representa la cantidad de amigos de la persona especificada. 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs usuario = cantidadDeElementosLista (amigosDe rs usuario)

-- Hago una funcion auxiliar que cuente la cantidad de elementos que tengo en una lista
cantidadDeElementosLista :: (Eq t) => [t] -> Int
cantidadDeElementosLista [] = 0
cantidadDeElementosLista [x] = 1
cantidadDeElementosLista (x:xs) = 1 + cantidadDeElementosLista xs 

-- Devuelve el usuario que mas amigos tenga en toda la red social
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = comparadorDeAmigos rs (usuarios rs)

-- Hago una funcion auxiliar para comparar la cantidad de amigos de cada usuario y ver cual tiene mas
comparadorDeAmigos :: RedSocial -> [Usuario] -> Usuario
comparadorDeAmigos _ [x] = x
comparadorDeAmigos rs (x:xs) | (cantidadDeAmigos rs x) >= cantidadDeAmigos rs (comparadorDeAmigos rs xs) = x
                             | otherwise = comparadorDeAmigos rs xs

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

tieneUnSeguidorFiel' :: RedSocial -> Usuario -> [Usuario] -> Bool
tieneUnSeguidorFiel' _ _ [] = False
tieneUnSeguidorFiel' rsRed uUsuario (u:us)
    | publicacionesDe rsRed uUsuario == []                                               = False -- Siempre devolvia True si un usuario no tenia publicaciones, pues esSubconjunto [] [a,b,...] siempre devuelve True
    | uUsuario == u                                                                      = tieneUnSeguidorFiel' rsRed uUsuario us
    | esSubconjunto (publicacionesDe rsRed uUsuario) (publicacionesQueLeGustanA rsRed u) = True
    | otherwise                                                                          = tieneUnSeguidorFiel' rsRed uUsuario us

-- Verifica que TODOS los elementos de una lista A pertenecen a una lista B
esSubconjunto :: (Eq t) => [t] -> [t] -> Bool
esSubconjunto _ [] = False
esSubconjunto [] _ = True
esSubconjunto (x:xs) (y:ys)
    | pertenece x (y:ys) = esSubconjunto xs (y:ys)
    | otherwise          = False

-- Toma un redSocial y dos usuarios, y verifica si existe una cadena de amigos que relaciona
-- indirectamente a estos usuarios, por ejemplo:
-- U1 es amigo de U2, U2 es amigo de U3
-- Si hacemos "existeSecuenciaDeAmigos <RedSocial> U1 U3", nos devolvera verdadero, pues hay
-- una cadena de amigos entre U1 y U3
-- ## MARCADA_PARA_ELIMINAR
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rsRed uU1 uU2
    | (pertenece uU1 (usuarios rsRed) && pertenece uU2 (usuarios rsRed)) == False = False
    | otherwise = existeSecuenciaDeAmigos' (usuarios rsRed) uU1 uU2 rsRed
-- ## MARCADA_PARA_ELIMINAR
existeSecuenciaDeAmigos' :: [Usuario] -> Usuario -> Usuario -> RedSocial -> Bool
existeSecuenciaDeAmigos' us uU1 uU2 rsRed
    | not (empiezaCon uU1 us) = existeSecuenciaDeAmigos' (moverAlPrincipio uU1 us) uU1 uU2 rsRed
    | not (terminaCon uU2 us) = existeSecuenciaDeAmigos' (moverAlFinal uU2 us) uU1 uU2 rsRed
    | otherwise               = cadenaDeAmigos us rsRed

-- ## MARCADA_PARA_ELIMINAR
-- Verifica si el primer elemento de una lista [t] es t
empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon x (y:_) = x == y

lista1 = []
-- Toma un elemento t y una lista [t] y :
-- Si el elemento t existe en la lista, toma el elemento mas a la izquierda de la lista y lo mueve al principio
-- Si el elemento t no existe en la lista, agrega t al principio de la lista.
moverAlPrincipio :: (Eq t) => t -> [t] -> [t]
moverAlPrincipio x []     = [x]
moverAlPrincipio x (y:ys) = x:quitar x (y:ys)

-- ## MARCADA_PARA_ELIMINAR
-- Verifica si el último elemento de una lista [t] es t
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon x (y:ys) = x == ultimoElem (y:ys)

-- Devuelve el último elemento de una lista
ultimoElem :: (Eq t) => [t] -> t
ultimoElem [x] = x
ultimoElem (x:xs) = ultimoElem xs

-- Toma un elemento t y una lista [t] y :
-- Si el elemento t existe en la lista, toma el elemento mas a la izquierda de la lista y lo mueve al fondo
-- Si el elemento t no existe en la lista, agrega t al final de la lista.
moverAlFinal :: (Eq t) => t -> [t] -> [t]
moverAlFinal x []     = [x]
moverAlFinal x (y:ys) = quitar x (y:ys) ++ [x]

-- Borra el primer t (empezando desde la izquierda).
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys)
    | x == y = ys
    | otherwise = y:quitar x ys

-- ## MARCADA_PARA_ELIMINAR
-- Borra varios elementos de un conjunto t (empezando desde la izquierda)
quitarVarios :: (Eq t) => [t] -> [t] -> [t]
quitarVarios _ [] = []
quitarVarios [] t = t
quitarVarios (x:xs) ys = quitarVarios xs (quitar x ys)
-- Toma una lista de usuarios liU y un RedSocial y verifica si existe una cadena de amigos entre
-- todos los Usuarios en liU.
-- Por ejemplo [U1, U2, U3, U4], si U1 esta relacionado con U2, U2 con U3 y U3 con U4, entonces
-- devuelve verdadero.
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [u1, u2] rsRed = relacionadosDirecto u1 u2 rsRed
cadenaDeAmigos (u1:u2:us) rsRed
    | cantidadDeAmigos rsRed u1 == 0  = False
    | not (perteneceAlgunElemDe (amigosDe rsRed u2) us) && relacionadosDirecto u1 u2 rsRed = cadenaDeAmigos (u1:us) rsRed
    | relacionadosDirecto u1 u2 rsRed                                                      = cadenaDeAmigos (u2:us) rsRed
    | otherwise                                                                            = cadenaDeAmigos ((u1:us) ++ [u2]) rsRed


 
-- Toma dos usuarios U1, U2 y toma una RedSocial y verifica si existe, una dupla Relación
-- en RedSocial que contiene U1 y U2 como sus elementos.
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto uU1 uU2 rsRed = pertenece (uU1, uU2) (relaciones rsRed) || pertenece (uU2, uU1) (relaciones rsRed)

-- Verificia si por lo menos un elemento de una lista [t], pertenece a otra lista [t]
perteneceAlgunElemDe :: (Eq t) => [t] -> [t] -> Bool
perteneceAlgunElemDe _ []    = False
perteneceAlgunElemDe [] _    = False
perteneceAlgunElemDe [x] liY = pertenece x liY
perteneceAlgunElemDe (x:xs) liY
    | pertenece x liY = True
    | otherwise       = perteneceAlgunElemDe xs liY

existeSecuenciaDeAmigos2 :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos2 rsRed uU1 uU2
    | (pertenece uU1 (usuarios rsRed) && pertenece uU2 (usuarios rsRed)) == False = False
    | otherwise = existeSecuenciaDeAmigos2' (usuarios rsRed) uU1 uU2 rsRed

existeSecuenciaDeAmigos2' :: [Usuario] -> Usuario -> Usuario -> RedSocial -> Bool
existeSecuenciaDeAmigos2' us uU1 uU2 rsRed
    | not (pertenece uU2 (usuarios rsRed)) || not (pertenece uU1 (usuarios rsRed)) = False
    | cantidadDeElementosLista (quitar uU2 (amigosDe rsRed uU1)) == 0 || cantidadDeElementosLista (quitar uU1 (amigosDe rsRed uU2)) == 0 = False
    | otherwise               = cadenaDeAmigos2 (quitar uU2 (amigosDe rsRed uU1)) rsRed

principio :: (Eq t) => [t] -> [t]
principio [x] = []
principio (x:xs) = x:principio xs

cadenaDeAmigos2 :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos2 [u1, u2] rsRed = relacionadosDirecto u1 u2 rsRed
cadenaDeAmigos2 (u1:u2:us) rsRed
    | cantidadDeAmigos rsRed u1 == 0                                                       = False
    | not (perteneceAlgunElemDe (amigosDe rsRed u2) us) && relacionadosDirecto u1 u2 rsRed = cadenaDeAmigos2 (u1:us) rsRed
    | relacionadosDirecto u1 u2 rsRed                                                      = cadenaDeAmigos2 (u2:us) rsRed
    | otherwise                                                                            = cadenaDeAmigos2 ((u1:us) ++ [u2]) rsRed
