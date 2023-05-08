module Solucion where
-- Ejercicios

-- Devuelve un [String] cuyos elementos son los nombres de cada usuario de la red
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = eliminarRepetidos (proyectarNombres (usuarios rs))

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

-- proyectarNombres: Toma una lista de Usuarios y devuelve
-- una lista de String donde cada elemento es el nombre 
-- de usuario de cada uno de los elementos de la lista de
-- Usuarios.
proyectarNombres :: [Usuario] -> [String]
proyectarNombres []     = [] 
proyectarNombres [u]    = [nombreDeUsuario u]
proyectarNombres (u:us) = [nombreDeUsuario u] ++ proyectarNombres us

-- pertenece: Verifica si un elemento pertenece
-- a una lista de elementos del mismo tipo. 
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x [y] = x == y
pertenece x (y:ys)
    | x == y    = True
    | otherwise = pertenece x ys

-- tieneRepetidos: Verifica si una lista tiene una elemento
-- mas de una véz
tieneRepetidos :: (Eq t) => [t] -> Bool
tieneRepetidos [] = False
tieneRepetidos [x] = False
tieneRepetidos (x:xs)
    | pertenece x xs  = True
    | otherwise       = tieneRepetidos xs

-- eliminarRepetidos: Toma una lista de algun tipo
-- y la devuelve pero cada elemento aparece una única vez.
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos []  = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs)
    | not (tieneRepetidos (x:xs)) = (x:xs)
    | pertenece x xs              = eliminarRepetidos xs
    | otherwise                   = x:eliminarRepetidos xs

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
