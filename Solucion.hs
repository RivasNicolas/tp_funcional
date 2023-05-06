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

-- Devuelve los nombres de usuarios en una lista: ["nombre1", "nombre2", ...]
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = nombresDeUsuarios' (usuarios rs)

nombresDeUsuarios' :: [Usuario] -> [String]
nombresDeUsuarios' []     = []
nombresDeUsuarios' [u]    = [nombreDeUsuario u]
nombresDeUsuarios' (u:us) = [nombreDeUsuario u] ++ nombresDeUsuarios' us

-- Duevuelve una lista de usuarios que estan relacionados con un usuario especifico.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs usuario = amigosDe' (relaciones rs) usuario

amigosDe' :: [Relacion] -> Usuario -> [Usuario]
amigosDe' [] _ = []
amigosDe' ((u1,u2):[]) usuario
    | usuario == u1 = [u2]
    | usuario == u2 = [u1]
    | otherwise     = []
amigosDe' ((u1,u2):rs) usuario
    | usuario == u1 = [u2] ++ amigosDe' rs usuario
    | usuario == u2 = [u1] ++ amigosDe' rs usuario
    | otherwise     = amigosDe' rs usuario

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

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
