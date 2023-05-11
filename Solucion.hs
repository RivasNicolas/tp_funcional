module Solucion where

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

-- Ejercicios

-- Devuelve un [String] cuyos elementos son los nombres de cada usuario de la red
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = eliminarRepetidos (proyectarNombres (usuarios rs)) --Ojo que en la línea 20 rs se usa para llamar a las relaciones.
                                                                          --Si bien funciona, no es bueno para el lector que dos variables que tienen el mismo nombre hagan referencia
                                                                          --a cosas distintas
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
cantidadDeElementosLista :: [Usuario] -> Int
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

-- Toma un RedSocial, un Usuario y devuelve un [Publicacion], este lo devuelve la función auxiliar.
-- Obtiene todas las publicaciones de la red y se lo envía a la función auxiliar junto al usuario.
-- Esta luego devolverá las publicaciones que le gustan al usuario, sin repetidos
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rsRed us = eliminarRepetidos (publicacionesQueLeGustanAAux (publicaciones rsRed) us) --Revisar si el eliminarRepetidos es necesario

-- Toma una lista de publicaciones y un usuario.
-- Si el usuario pertenece a la lista de usuarios que le dieron like a la publicacion, esta se agrega a la lista a devolver.
publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux [pub] us | pertenece us (likesDePublicacion pub) = [pub]
    | otherwise                                                            = []
publicacionesQueLeGustanAAux (pub : pubs) us | pertenece us (likesDePublicacion pub) = pub : publicacionesQueLeGustanAAux pubs us
    | otherwise                                                                   = publicacionesQueLeGustanAAux pubs us

-- Toma un RedSocial, dos Usuarios y devuelve un Bool si a ambos les gustan las mismas publicaciones
-- A ambos les gustan las mismas publicaciones si la lista de publicaciones que le gustan al usuario1 está incluida en la lista de publicaciones que le gustan al usuario2,
-- y si la lista de publicaciones que le gustan al usuario2 está incluida en la lista de publicaciones que le gustan al usuario1
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rsRed us1 us2 = esSubconjunto (publicacionesQueLeGustanA rsRed us1) (publicacionesQueLeGustanA rsRed us2) &&
                                                esSubconjunto (publicacionesQueLeGustanA rsRed us2) (publicacionesQueLeGustanA rsRed us1)

-- Toma un RedSocial y un usuario, y verifica que existe un Usuario u1 tal que:
-- u1 pertenece a todas las listas de usuarios de las publicaciones de otro
-- diferente usuario u2
-- Es decir, u1 le dio me gusta a todas las publicaciones de u2
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rsRed uUsuario = tieneUnSeguidorFiel' rsRed uUsuario (usuarios rsRed)

tieneUnSeguidorFiel' :: RedSocial -> Usuario -> [Usuario] -> Bool
tieneUnSeguidorFiel' _ _ [] = False
tieneUnSeguidorFiel' rsRed uUsuario (u:us)
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
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rsRed uU1 uU2 = existeSecuenciaDeAmigos' (usuarios rsRed) uU1 uU2 rsRed

existeSecuenciaDeAmigos' :: [Usuario] -> Usuario -> Usuario -> RedSocial -> Bool
existeSecuenciaDeAmigos' us uU1 uU2 rsRed
    | not (empiezaCon uU1 us) = existeSecuenciaDeAmigos' (moverAlPrincipio uU1 us) uU1 uU2 rsRed
    | not (terminaCon uU2 us) = existeSecuenciaDeAmigos' (moverAlFinal uU2 us) uU1 uU2 rsRed
    | otherwise               = cadenaDeAmigos us rsRed

-- Verifica si el primer elemento de una lista [t] es t
empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon x (y:_) = x == y

-- Toma un elemento t y una lista [t] y :
-- Si el elemento t existe en la lista, toma el elemento mas a la izquierda de la lista y lo mueve al principio
-- Si el elemento t no existe en la lista, agrega t al principio de la lista.
moverAlPrincipio :: (Eq t) => t -> [t] -> [t]
moverAlPrincipio x []     = [x]
moverAlPrincipio x (y:ys) = x:quitar x (y:ys)

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