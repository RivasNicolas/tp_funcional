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
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rsRed uU1 uU2
    | (pertenece uU1 (usuarios rsRed) && pertenece uU2 (usuarios rsRed)) == False = False
    | otherwise = existeSecuenciaDeAmigos' (usuarios rsRed) uU1 uU2 rsRed

existeSecuenciaDeAmigos' :: [Usuario] -> Usuario -> Usuario -> RedSocial -> Bool
existeSecuenciaDeAmigos' us uU1 uU2 rsRed
    | pertenece uU1 (usuarios rsRed) && pertenece uU2 (usuarios rsRed) == False = False
    | otherwise               = cadenaDeAmigos uU1 uU2 us rsRed

-- Devuelve el último elemento de una lista
ultimoElem :: (Eq t) => [t] -> t
ultimoElem [x] = x
ultimoElem (x:xs) = ultimoElem xs

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
cadenaDeAmigos :: Usuario -> Usuario -> [Usuario] -> RedSocial -> Bool
cadenaDeAmigos u0 un [u1, u2] rsRed = relacionadosDirecto u1 u2 rsRed
cadenaDeAmigos u0 un (u1:u2:us) rsRed
    | cantidadDeAmigos rsRed u1 == 0  = False
    | not (perteneceAlgunElemDe (amigosDe rsRed u2) us) && relacionadosDirecto u1 u2 rsRed && u1 == u0 = False
    | not (perteneceAlgunElemDe (amigosDe rsRed u1) us) && relacionadosDirecto u1 u2 rsRed && u2 == un = False
    | not (perteneceAlgunElemDe (amigosDe rsRed u2) us) && relacionadosDirecto u1 u2 rsRed             = cadenaDeAmigos u0 un (u1:us) rsRed
    | relacionadosDirecto u1 u2 rsRed && u2 == un                                                      = cadenaDeAmigos u0 un (u1:us) rsRed
    | relacionadosDirecto u1 u2 rsRed                                                                  = cadenaDeAmigos u0 un (u2:us) rsRed
    | otherwise                                                                                        = cadenaDeAmigos u0 un ((u1:us) ++ [u2]) rsRed
 
-- Toma dos usuarios U1, U2 y toma una RedSocial y verifica si existe, una dupla Relación
-- en RedSocial que contiene a U1 y U2 como sus elementos.
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

-- Devuelve todos los elementos de una lista excepto el último
principio :: (Eq t) => [t] -> [t]
principio [x] = []
principio (x:xs) = x:principio xs