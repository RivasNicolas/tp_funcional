import Test.HUnit
import Solucion

main = runTestTT tests

-- Todos los tests que incluyen un 1 en su nombre, venían con la consigna

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    --" nombresDeUsuarios" ~: testsEj1,
    " amigosDe" ~: testsEj2,
    " cantidadDeAmigos" ~: testsEj3,
    " usuarioConMasAmigos" ~: testsEj4,
    " estaRobertoCarlos" ~: testsEj5,
    " publicacionesDe" ~: testsEj6,
    " publicacionesQueLeGustanA" ~: testsEj7,
    " leGustanLasMismasPublicaciones" ~: testsEj8
    --" tieneUnSeguidorFiel" ~: testsEj9,
    --" existeSecuenciaDeAmigos" ~: testsEj10
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos
--nombresDeUsuarios :: RedSocial -> [String]
--nombresDeUsuarios :: RedSocial -> [String]

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-- Test realizados por nosotros --
testsEj2 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red tiene relaciones que no contienen a u. Red contiene a u." ~: amigosDe red2 usuario_2_4 ~?= [],
    "Caso 3: red tiene relaciones no repetidas y contiene a u." ~: amigosDe red2 usuario_2_1 ~?= [usuario_2_2, usuario_2_3],
    --"Caso 4: red tiene relaciones repetidas y contiene a u." No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 5: red no tiene relaciones. Red contiene a u." ~: amigosDe red2SinRelaciones usuario_2_2 ~?= []
    ]
usuario_2_1 = (1, "Juan Ignacio")
usuario_2_2 = (2, "Ezequiel")
usuario_2_3 = (3, "Benjamin")
usuario_2_4 = (4, "Roberto Carlos")
usuarios2 = [usuario_2_1, usuario_2_2, usuario_2_3, usuario_2_4]
relacion2_12 = (usuario_2_1, usuario_2_2)
relacion2_31 = (usuario_2_3, usuario_2_1)
relaciones2 = [relacion2_12, relacion2_31]
red2 = (usuarios2, relaciones2, [])
red2SinRelaciones = (usuarios2, [], [])

testsEj3 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red tiene relaciones que no contienen a u. Red contiene a u." ~: cantidadDeAmigos red2 usuario_2_4 ~?= 0,
    "Caso 3: red tiene relaciones no repetidas y contiene a u." ~: cantidadDeAmigos red2 usuario_2_1 ~?= 2,
    --"Caso 4: red tiene relaciones repetidas y contiene a u." No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 5: red no tiene relaciones. Red contiene a u." ~: cantidadDeAmigos red2SinRelaciones usuario_2_2 ~?= 0
    ]

testsEj4 = test [
    --"Caso 1: la cantidad de usuarios de la red es 0"  No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: la cantidad de usuarios de la red es mayor a 1 y red no tiene relaciones" ~: usuarioConMasAmigos red2SinRelaciones ~?= usuario_2_1,
    "Caso 3: la cantidad de usuarios de la red es mayor a 1 y red tiene relaciones, ninguna repetida" ~: usuarioConMasAmigos red2 ~?= usuario_2_1
    --"Caso 4: la cantidad de usuarios de la red es mayor a 1 y red tiene relaciones, con repetidas" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    ]

testsEj5 = test [
    "Caso 1: La cantidad máxima de amigos de un usuario es menor a 10." ~: estaRobertoCarlos red2SinRelaciones ~?= False,
    "Caso 2: La cantidad máxima de amigos de un usuario es 10." ~: estaRobertoCarlos red5SinRobertoCarlos ~?= False,
    "Caso 3: La cantidad máxima de amigos de un usuario es mayor a 10." ~: estaRobertoCarlos red5ConRobertoCarlos ~?= True
    ]
usuario_5_5 = (5, "Javier")
usuario_5_6 = (6, "Gabriel")
usuario_5_7 = (7, "Franco")
usuario_5_8 = (8, "Mariela")
usuario_5_9 = (9, "Natalia")
usuario_5_10 = (10, "Liliana")
usuario_5_11 = (11, "Miguel")
usuario_5_12 = (12, "Eda")
usuarios5 = [usuario_2_1, usuario_2_2, usuario_2_3, usuario_2_4, usuario_5_5, usuario_5_6, usuario_5_7, usuario_5_8, usuario_5_9, usuario_5_10, usuario_5_11, usuario_5_12]
relacion_5_41 = (usuario_2_4, usuario_2_1)
relacion_5_42 = (usuario_2_4, usuario_2_2)
relacion_5_43 = (usuario_2_4, usuario_2_3)
relacion_5_45 = (usuario_2_4, usuario_5_5)
relacion_5_46 = (usuario_2_4, usuario_5_6)
relacion_5_47 = (usuario_2_4, usuario_5_7)
relacion_5_48 = (usuario_2_4, usuario_5_8)
relacion_5_49 = (usuario_2_4, usuario_5_9)
relacion_5_410 = (usuario_2_4, usuario_5_10)
relacion_5_411 = (usuario_2_4, usuario_5_11)
relacion_5_412 = (usuario_2_4, usuario_5_12)
relaciones5Con10Amigos = [relacion_5_41, relacion_5_42, relacion_5_43, relacion_5_45, relacion_5_46, relacion_5_47, relacion_5_48, relacion_5_49, relacion_5_410, relacion_5_411]
relaciones5Con11Amigos = [relacion_5_41, relacion_5_42, relacion_5_43, relacion_5_45, relacion_5_46, relacion_5_47, relacion_5_48, relacion_5_49, relacion_5_410, relacion_5_411
                         , relacion_5_412]
red5SinRobertoCarlos = (usuarios5, relaciones5Con10Amigos, [])
red5ConRobertoCarlos = (usuarios5, relaciones5Con11Amigos, [])

testsEj6 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red no tiene publicaciones" ~: publicacionesDe red5ConRobertoCarlos usuario_2_4 ~?= [],
    "Caso 3: red tiene publicaciones y u no." ~: publicacionesDe red6 usuario_2_3 ~?= [],
    --"Caso 4: red tiene publicaciones y u tiene repetidas" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 5: red tiene publicaciones y u tiene, sin repetidas" ~: publicacionesDe red6 usuario_2_1 ~?= [publicacion6_1, publicacion6_2, publicacion6_3, publicacion6_4, publicacion6_5]
    ]
publicacion6_1 = (usuario_2_1, "Este es mi primer post", [usuario_2_2, usuario_2_3, usuario_2_4])
publicacion6_2 = (usuario_2_1, "Este es mi segundo post", [usuario_2_2, usuario_2_3, usuario_2_4])
publicacion6_3 = (usuario_2_1, "Este es mi tercer post", [usuario_2_2])
publicacion6_4 = (usuario_2_1, "Este es mi cuarto post", [usuario_2_2])
publicacion6_5 = (usuario_2_1, "Este es como mi quinto post", [usuario_2_2])
publicacion6_6 = (usuario_2_4, "Hola red", [usuario_2_1])
publicacion6_7 = (usuario_2_2, "Hello world", [])
publicaciones6 = [publicacion6_1, publicacion6_2, publicacion6_3, publicacion6_4, publicacion6_5, publicacion6_6, publicacion6_7]
red6 = (usuarios2, relaciones2, publicaciones6)

testsEj7 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red no tiene publicaciones" ~: publicacionesQueLeGustanA red5ConRobertoCarlos usuario_2_4 ~?= [],
    "Caso 3: red tiene publicaciones y a u no le gusta ninguna" ~: publicacionesQueLeGustanA redB usuario1 ~?= [],
    "Caso 4: Caso 4: red tiene publicaciones y a u le gusta alguna" ~: publicacionesQueLeGustanA redB usuario2 ~?= [publicacion1_3, publicacion3_2, publicacion3_3]
    ]

testsEj8 = test[
    --"Caso 1: red no contiene a u1 o u2" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red no tiene publicaciones" ~: lesGustanLasMismasPublicaciones red5ConRobertoCarlos usuario_2_2 usuario_2_4 ~?= True,
    "Caso 3: red tiene publicaciones y ni a u1 ni a u2 le gusta ninguna" ~: lesGustanLasMismasPublicaciones redB usuario1 usuario3 ~?= True,
    "Caso 4: red tiene publicaciones y uno solo de los dos usuarios tiene publicaciones que le gustan, mientras el otro no tiene ninguna" ~: lesGustanLasMismasPublicaciones redB usuario2 usuario1 ~?= False,
    "Caso 5: red tiene publicaciones y ambos usuarios tienen publicaciones que les gustan en común pero, también tienen algunas que les gustan a ellos solos" ~: lesGustanLasMismasPublicaciones redB usuario2 usuario5 ~?= False,
    "Caso 6: red tiene publicaciones y a u1 y u2 les gustan todas las mismas" ~: lesGustanLasMismasPublicaciones red6 usuario_2_3 usuario_2_4 ~?= True
    ]