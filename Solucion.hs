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
nombresDeUsuarios rsRed = eliminarRepetidos (proyectarNombres (usuarios rsRed))

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
    
-- tieneRepetidos: Verifica si una lista tiene un elemento
-- mas de una vez
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
amigosDe rsRed uUsuario = amigosDeAux (relaciones rsRed) uUsuario

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux [(u1,u2)] uUsuario
    | uUsuario == u1 = [u2]
    | uUsuario == u2 = [u1]
    | otherwise      = []
amigosDeAux ((u1,u2):rRelaciones) uUsuario
    | uUsuario == u1 = u2:amigosDeAux rRelaciones uUsuario
    | uUsuario == u2 = u1:amigosDeAux rRelaciones uUsuario
    | otherwise      = amigosDeAux rRelaciones uUsuario

-- Devuelve un entero que representa la cantidad de amigos de la persona especificada. 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rsRed usuario = cantidadDeElementosLista (amigosDe rsRed usuario)

-- Hago una funcion auxiliar que cuente la cantidad de elementos que tengo en una lista
cantidadDeElementosLista :: (Eq t) => [t] -> Int
cantidadDeElementosLista [] = 0
cantidadDeElementosLista [x] = 1
cantidadDeElementosLista (x:xs) = 1 + cantidadDeElementosLista xs 

-- Devuelve el usuario que mas amigos tenga en toda la red social
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rsRed = comparadorDeAmigos rsRed (usuarios rsRed)

-- Hago una funcion auxiliar para comparar la cantidad de amigos de cada usuario y ver cual tiene mas
comparadorDeAmigos :: RedSocial -> [Usuario] -> Usuario
comparadorDeAmigos _ [x] = x
comparadorDeAmigos rsRed (x:xs) | (cantidadDeAmigos rsRed x) >= cantidadDeAmigos rsRed (comparadorDeAmigos rsRed xs) = x
                             | otherwise = comparadorDeAmigos rsRed xs

-- Determina si es que hay un usuario en la red social que tenga más de 10 amigos exactamente
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rsRed = estaRobertoCarlosAux rsRed (usuarios rsRed)

--Funcion auxiliar para que cuente si la cantidad de amigos de cada usuario es mayor a 10
estaRobertoCarlosAux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosAux rsRed us | us == [] = False
                           | cantidadDeAmigos rsRed (head us) > 10 = True
                           | otherwise = estaRobertoCarlosAux rsRed (tail us)

-- Toma un RedSocial y un Usuario u, devuelve un [Publicacion] de red social en las que u es el autor de cada Publicacion.
-- Es decir, las publicaciones escritas por u.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rsRed uUsuario = eliminarRepetidos(publicacionesDeAux (publicaciones rsRed) uUsuario)

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (p:ps) uUsuario
    | uUsuario == usuarioDePublicacion p = p:publicacionesDeAux ps uUsuario
    | otherwise                          = publicacionesDeAux ps uUsuario

-- Obtiene todas las publicaciones de la red y se lo envía a la función auxiliar junto al usuario.
-- Esta luego devolverá las publicaciones que le gustan al usuario, sin repetidos
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rsRed us = publicacionesQueLeGustanAAux (publicaciones rsRed) us
-- eliminarRepetidos no es necesario, solo se pueden repetir si la red tiene la misma publicacion
-- repetida dos o mas veces, en tal caso, la red no es valida por las especificación.

-- Toma una lista de publicaciones y un usuario.
-- Si el usuario pertenece a la lista de usuarios que le dieron like a la publicacion, esta se agrega a la lista a devolver.
publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux [pub] us | pertenece us (likesDePublicacion pub) = [pub]
    | otherwise                                                               = []
publicacionesQueLeGustanAAux (pub : pubs) us | pertenece us (likesDePublicacion pub) = pub : publicacionesQueLeGustanAAux pubs us
    | otherwise                                                                      = publicacionesQueLeGustanAAux pubs us

-- Toma un RedSocial, dos Usuarios y devuelve un Bool si a ambos les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rsRed us1 us2 = lesGustanLasMismasPublicacionesAux (publicacionesQueLeGustanA rsRed us1) (publicacionesQueLeGustanA rsRed us2)

-- A ambos les gustan las mismas publicaciones si la lista de publicaciones que le gustan al usuario1 está incluida en la lista de publicaciones 
-- que le gustan al usuario2, y viceversa.
lesGustanLasMismasPublicacionesAux :: [Publicacion] -> [Publicacion] -> Bool    
lesGustanLasMismasPublicacionesAux [] [] = True 
lesGustanLasMismasPublicacionesAux likesUs1 likesUs2 = esSubconjunto likesUs1 likesUs2 && esSubconjunto likesUs2 likesUs1

-- Toma un RedSocial y un usuario, y verifica que existe un Usuario u1 tal que:
-- u1 pertenece a todas las listas de usuarios de las publicaciones de otro diferente usuario u2
-- Es decir, u1 le dio me gusta a todas las publicaciones de u2
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rsRed uUsuario = tieneUnSeguidorFielAux rsRed uUsuario (usuarios rsRed)

-- Case publicacionesDe rsRed uUsuario == []:
-- Siempre devolvia True si un usuario no tenia publicaciones, pues esSubconjunto [] [a,b,...] siempre devuelve True
tieneUnSeguidorFielAux :: RedSocial -> Usuario -> [Usuario] -> Bool
tieneUnSeguidorFielAux _ _ [] = False
tieneUnSeguidorFielAux rsRed uUsuario (u:us)
    | publicacionesDe rsRed uUsuario == []                                               = False
    | uUsuario == u                                                                      = tieneUnSeguidorFielAux rsRed uUsuario us
    | esSubconjunto (publicacionesDe rsRed uUsuario) (publicacionesQueLeGustanA rsRed u) = True
    | otherwise                                                                          = tieneUnSeguidorFielAux rsRed uUsuario us
    
-- Verifica que TODOS los elementos de una lista A pertenecen a una lista B
esSubconjunto :: (Eq t) => [t] -> [t] -> Bool
esSubconjunto _ [] = False
esSubconjunto [] _ = True
esSubconjunto (x:xs) (y:ys)
    | pertenece x (y:ys) = esSubconjunto xs (y:ys)
    | otherwise          = False

-- Toma un redSocial y dos usuarios, y verifica si existe una cadena de amigos que relaciona
-- a estos usuarios, por ejemplo:
-- U1 es amigo de U2, U2 es amigo de U3
-- Si hacemos "existeSecuenciaDeAmigos <RedSocial> U1 U3", nos devolvera verdadero, pues hay
-- una cadena de amigos entre U1 y U3
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rsRed uU1 uU2
    | (pertenece uU1 (usuarios rsRed) && pertenece uU2 (usuarios rsRed)) == False  = False
    | (cantidadDeAmigos rsRed uU1 == 0 || cantidadDeAmigos rsRed uU2 == 0) == True = False
    | (relacionadosDirecto uU1 uU2 rsRed) == True                                  = True
    | otherwise = cadenaDeAmigos (amigosDe rsRed uU1) uU2 rsRed [uU1]

-- Borra el primer t (empezando desde la izquierda).
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys)
    | x == y = ys
    | otherwise = y:quitar x ys

-- Toma dos usuarios U0 y UN, una lista de usuarios liU y un RedSocial y verifica si existe una cadena de amigos empieza con U1
-- termina con U2.
-- Por ejemplo [U0, U1, U2, UN], si U0 esta relacionado con U1, U1 con U2 y U2 con UN, entonces
-- devuelve verdadero.
cadenaDeAmigos :: [Usuario] -> Usuario -> RedSocial -> [Usuario] -> Bool
cadenaDeAmigos [] _ _ _ = False
cadenaDeAmigos [amU1] un rsRed _ = (relacionadosDirecto amU1 un rsRed || interseccion (amigosDe rsRed amU1) (amigosDe rsRed un))
cadenaDeAmigos (amU1:amU1s) un rsRed yaTesteado
    | pertenece amU1 yaTesteado = cadenaDeAmigos amU1s un rsRed yaTesteado
    | (cantidadDeAmigos rsRed amU1) == 1                                            = cadenaDeAmigos amU1s un rsRed (yaTesteado ++ [amU1])
    | cadenaDeAmigos (amigosDe rsRed amU1) un rsRed (yaTesteado ++ [amU1]) == False = cadenaDeAmigos amU1s un rsRed (yaTesteado ++ [amU1])
    | otherwise                                                                     = cadenaDeAmigos (amigosDe rsRed amU1) un rsRed (yaTesteado ++ [amU1])


-- Toma dos usuarios U1, U2 y toma una RedSocial y verifica si existe, una dupla Relación
-- en RedSocial que contiene U1 y U2 como sus elementos.
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto uU1 uU2 rsRed = pertenece (uU1, uU2) (relaciones rsRed) || pertenece (uU2, uU1) (relaciones rsRed)

-- Verifica si por lo menos un elemento de una lista [t], pertenece a otra lista [t]
interseccion :: (Eq t) => [t] -> [t] -> Bool
interseccion _ []    = False
interseccion [] _    = False
interseccion [x] liY = pertenece x liY
interseccion (x:xs) liY
    | pertenece x liY = True
    | otherwise       = interseccion xs liY
    
