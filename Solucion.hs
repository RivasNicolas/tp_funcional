module Solucion where
import TP

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

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

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
