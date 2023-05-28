import Test.HUnit
import Solucion

-- Nombre de Grupo: Cambalache
-- Integrante 1: Ezequiel Juan Fernández, ezequieljuanfernandez2003@gmail.com, 774/23
-- Integrante 2: Tomas Benjamin Ramirez, rtb.fcen@gmail.com, 530/23
-- Integrante 3: Nicolás Rivas, nicolas@rivasgarcia.com.ar, 321/23
-- Integrante 4: Juan Ignacio Villalba, juaninator360@gmail.com, 759/23

main = runTestTT tests

tests = test [
    " nombresDeUsuarios" ~: testsEj1,
    " amigosDe" ~: testsEj2,
    " cantidadDeAmigos" ~: testsEj3,
    " usuarioConMasAmigos" ~: testsEj4,
    " estaRobertoCarlos" ~: testsEj5,
    " publicacionesDe" ~: testsEj6,
    " publicacionesQueLeGustanA" ~: testsEj7,
    " leGustanLasMismasPublicaciones" ~: testsEj8,
    " tieneUnSeguidorFiel" ~: testsEj9,
    " existeSecuenciaDeAmigos" ~: testsEj10
    ]

--Test por cada ejercicio

-- Los casos de prueba estan formados en base a la fuerza de los argumentos:
-- Por ejemplo, "La red A no tiene publicaciones" fuerza a "Usuario A no tiene publicaciones"
-- Pero "Usuario A no tiene publicaciones" no fuerza a "La red no tiene publicaciones", pues la red
-- podría tener otro usuario que tenga publicaciones.
testsEj1 = test [
    "Caso 1: La red no tiene usuarios" ~: nombresDeUsuarios redVacia  ~?= [],
    "Caso 2: La red tiene mas de un usuario" ~: nombresDeUsuarios redA ~?= ["Juan","Natalia","Pedro","Mariela"],
    "Caso 3: La red tiene usuarios con nombres repetidos" ~: nombresDeUsuarios redB ~?= ["Juan", "Pedro", "Natalia"]
    ]

testsEj2 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red tiene relaciones que no contienen a u. Red contiene a u." ~: amigosDe redSinPublicaciones usuario4 ~?= [],
    "Caso 3: red tiene relaciones no repetidas y contiene a u." ~: amigosDe redSinPublicaciones usuario1 ~?= [usuario2, usuario3],
    --"Caso 4: red tiene relaciones repetidas y contiene a u." No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 5: red no tiene relaciones. Red contiene a u." ~: amigosDe redSinRelaciones usuario1 ~?= []
    ]

testsEj3 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red tiene relaciones que no contienen a u. Red contiene a u." ~: cantidadDeAmigos redSinPublicaciones usuario4 ~?= 0,
    "Caso 3: red tiene relaciones no repetidas y contiene a u." ~: cantidadDeAmigos redSinPublicaciones usuario1 ~?= 2,
    --"Caso 4: red tiene relaciones repetidas y contiene a u." No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 5: red no tiene relaciones. Red contiene a u." ~: cantidadDeAmigos redSinRelaciones usuario1 ~?= 0
    ]

testsEj4 = test [
    --"Caso 1: la cantidad de usuarios de la red es 0"  No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: la cantidad de usuarios de la red es mayor a 1 y red no tiene relaciones" ~: expectAny (usuarioConMasAmigos redSinRelaciones) usuariosA,
    "Caso 3: la cantidad de usuarios de la red es mayor a 1 y red tiene relaciones, ninguna repetida" ~: usuarioConMasAmigos redSinPublicaciones ~?= usuario1
    --"Caso 4: la cantidad de usuarios de la red es mayor a 1 y red tiene relaciones, con repetidas" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

testsEj5 = test [
    "Caso 1: La cantidad máxima de amigos de un usuario es menor a 10." ~: estaRobertoCarlos redSinRelaciones ~?= False,
    "Caso 2: La cantidad máxima de amigos de un usuario es 10." ~: estaRobertoCarlos redSinRobertoCarlos ~?= False,
    "Caso 3: La cantidad máxima de amigos de un usuario es mayor a 10." ~: estaRobertoCarlos redConRobertoCarlos ~?= True
    ]

testsEj6 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red no tiene publicaciones" ~: publicacionesDe redSinPublicaciones usuario1 ~?= [],
    "Caso 3: red tiene publicaciones y u no." ~: publicacionesDe redB usuario5 ~?= [],
    --"Caso 4: red tiene publicaciones y u tiene repetidas" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 5: red tiene publicaciones y u tiene, sin repetidas" ~: publicacionesDe redA usuario1 ~?= [publicacion1_1, publicacion1_2]
    ]

testsEj7 = test [
    --"Caso 1: red no contiene a u" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red no tiene publicaciones" ~: publicacionesQueLeGustanA redSinPublicaciones usuario1 ~?= [],
    "Caso 3: red tiene publicaciones y a u no le gusta ninguna" ~: publicacionesQueLeGustanA redB usuario1 ~?= [],
    "Caso 4: Caso 4: red tiene publicaciones y a u le gusta alguna" ~: publicacionesQueLeGustanA redB usuario2 ~?= [publicacion1_3, publicacion3_2, publicacion3_3]
    ]

testsEj8 = test[
    --"Caso 1: red no contiene a u1 o u2" No se testea porque se trata de un caso de error. No está especificado qué sucede.
    "Caso 2: red no tiene publicaciones" ~: lesGustanLasMismasPublicaciones redSinPublicaciones usuario1 usuario2 ~?= True,
    "Caso 3: red tiene publicaciones y ni a u1 ni a u2 le gusta ninguna" ~: lesGustanLasMismasPublicaciones redB usuario1 usuario3 ~?= True,
    "Caso 4: red tiene publicaciones y uno solo de los dos usuarios tiene publicaciones que le gustan, mientras el otro no tiene ninguna" ~: lesGustanLasMismasPublicaciones redB usuario2 usuario1 ~?= False,
    "Caso 5: red tiene publicaciones y ambos usuarios tienen publicaciones que les gustan en común pero, también tienen algunas que les gustan a ellos solos" ~: lesGustanLasMismasPublicaciones redB usuario2 usuario5 ~?= False,
    "Caso 6: red tiene publicaciones y a u1 y u2 les gustan todas las mismas" ~: lesGustanLasMismasPublicaciones redMismosLikes usuario2 usuario5 ~?= True
    ]

testsEj9 = test[
    "Caso 1: la red no tiene publicaciones"                                                    ~: tieneUnSeguidorFiel redTEST_EJ9A usuario900 ~?= False,
    "Caso 2: la red tiene menos de 2 usuarios diferentes"                                      ~: tieneUnSeguidorFiel redTEST_EJ9B usuario900 ~?= False,
    "Caso 3: el usuario no tiene publicaciones en esta red"                                    ~: tieneUnSeguidorFiel redTEST_EJ9C usuario903 ~?= False,
    "Caso 4: no existe un usuario u2 al que le gusten TODAS las publicaciones de u1"           ~: tieneUnSeguidorFiel redTEST_EJ9D usuario900 ~?= False,
    "Caso 5: existe por lo menos un usuario u2 al que le gustan TODAS las publicaciones de u1" ~: tieneUnSeguidorFiel redTEST_EJ9E usuario900 ~?= True
    ]
usuarioA  = (1, "A")
usuario51  = (2, "1")
usuario52  = (3, "2")
usuario53  = (4, "3")
usuario54  = (5, "4")
usuario55  = (6, "5")
usuario56  = (7, "6")
usuario57  = (8, "7")
usuario58  = (9, "8")
usuario59  = (10, "9")
usuario60 = (11, "10")
usuarioB  = (12, "B")

relacionA_51 = (usuarioA, usuario51)
relacionA_52 = (usuarioA, usuario52)
relacionA_54 = (usuarioA, usuario54)

relacion52_54 = (usuario52, usuario54)

relacion51_55 = (usuario51, usuario55)
relacion51_56 = (usuario51, usuario56)

relacion56_60 = (usuario56, usuario60)

relacion60_B = (usuario60, usuarioB)

relacion53_57 = (usuario53, usuario57)
relacion53_58 = (usuario53, usuario58)

relacionesTestEJ10J = [relacionA_51, relacionA_52, relacionA_54, relacion52_54, 
                       relacion51_55, relacion51_56, relacion56_60, relacion60_B, 
                       relacion53_57, relacion53_58]

usuariosTestEJ10J = [usuarioA, usuarioB, usuario51, usuario52, usuario53, 
                     usuario54, usuario55, usuario56, usuario57, usuario58,
                     usuario59, usuario60]

redTestEJ10J = (usuariosTestEJ10J, relacionesTestEJ10J, [])


testsEj10 = test [
    "Caso 1: La red no tiene usuarios"                ~: existeSecuenciaDeAmigos redTestEJ10A usuarioGenericoA usuarioGenericoB ~?= False,
    "Caso 2: La red no tiene relaciones"              ~: existeSecuenciaDeAmigos redTestEJ10B usuarioGenericoA usuarioGenericoB ~?= False,
    "Caso 3: El usuario 1001 no esta en la red"                  ~: existeSecuenciaDeAmigos redTestEJ10C usuario1000 usuario1001 ~?= False,
    "Caso 4: El usuario 1000 no esta en la red"                  ~: existeSecuenciaDeAmigos redTestEJ10D usuario1000 usuario1001 ~?= False,
    "Caso 5: El usuario 1002 no tiene amigos"                    ~: existeSecuenciaDeAmigos redTestEJ10E usuario1000 usuario1002 ~?= False,
    "Caso 6: El usuario 1000 no tiene amigos"                    ~: existeSecuenciaDeAmigos redTestEJ10F usuario1000 usuario1002 ~?= False,
    "Caso 7: No existe la secuencia de amigos"        ~: existeSecuenciaDeAmigos redTestEJ10G usuario1000 usuario1003 ~?= False,
    "Caso 8: Existe la secuencia de amigos"           ~: existeSecuenciaDeAmigos redTestEJ10H usuario1000 usuario1003 ~?= True,
    "Case permutado 1: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P1 usuario1000 usuario1002 ~?= True,
    "Case permutado 2: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P2 usuario1000 usuario1002 ~?= True,
    "Case permutado 3: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P3 usuario1000 usuario1002 ~?= True,
    "Case permutado 4: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P4 usuario1000 usuario1002 ~?= True,
    "Case permutado 5: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P5 usuario1000 usuario1002 ~?= True,
    "Case permutado 6: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P6 usuario1000 usuario1002 ~?= True,
    "Caso 8: robustez"                                ~: existeSecuenciaDeAmigos redTestEJ10J usuarioA usuarioB ~?= True
    ]

--Redes utilizadas para los tests
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Gabriel")
usuario7 = (7, "Franco")
usuario8 = (8, "Sofia")
usuario9 = (9, "Federico")
usuario10 = (10, "Liliana")
usuario11 = (11, "Miguel")
usuario12 = (12, "Eda")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)

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

usuarios5 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesSinRobertoCarlos = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9,
                             relacion1_10, relacion1_11]
redSinRobertoCarlos = (usuarios5, relacionesSinRobertoCarlos, [])

relacionesConRobertoCarlos = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9,
                             relacion1_10, relacion1_11, relacion1_12]
redConRobertoCarlos = (usuarios5, relacionesConRobertoCarlos, [])

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redVacia = ([], [], [])

relaciones2 = [relacion1_2, relacion1_3]
redSinPublicaciones = (usuariosA, relaciones2, [])
redSinRelaciones = (usuariosA, [], [])

publicacionesMismosLikes = [publicacion1_3, publicacion3_3] 
redMismosLikes = (usuariosB, [], publicacionesMismosLikes)

usuario900 = (1, "u1")
usuario901 = (2, "u2")
usuario902 = (3, "u3")
usuario903 = (4, "u4") -- Sin publicaiones
usuario904 = (5, "u5")

publicacion900_1 = (usuario900, "pA", []) -- Publicación sin likes
publicacion900_2 = (usuario900, "pB", [usuario900,usuario901])
publicacion900_3 = (usuario900, "pC", [usuario902])
publicacion900_4 = (usuario900, "pD", [usuario902])
publicacion900_5 = (usuario900, "pE", [usuario902])

usuariosTEST_EJ9A = [usuario900]
usuariosTEST_EJ9C = [usuario900, usuario901, usuario903]
usuariosTEST_EJ9D = [usuario900, usuario901, usuario902]
usuariosTEST_EJ9E = [usuario900, usuario902]

publicacionesTEST_EJ9B = [publicacion900_1]
publicacionesTEST_EJ9C = [publicacion900_1, publicacion900_2]
publicacionesTEST_EJ9D = [publicacion900_2, publicacion900_3, publicacion900_4]
publicacionesTEST_EJ9E = [publicacion900_3, publicacion900_4, publicacion900_5]

redTEST_EJ9A = (usuariosTEST_EJ9A,[],[])
redTEST_EJ9B = (usuariosTEST_EJ9A, [], publicacionesTEST_EJ9B)
redTEST_EJ9C = (usuariosTEST_EJ9C, [], publicacionesTEST_EJ9C)
redTEST_EJ9D = (usuariosTEST_EJ9D, [], publicacionesTEST_EJ9D)
redTEST_EJ9E = (usuariosTEST_EJ9E, [], publicacionesTEST_EJ9E)

usuario1000 = (1000, "A")
usuario1001 = (1001, "B")
usuario1002 = (1002, "C")
usuario1003 = (1003, "D")

relacion1000_1002 = (usuario1000, usuario1002)
relacion1000_1001 = (usuario1000, usuario1001)
relacion1001_1002 = (usuario1001, usuario1002)
relacion1002_1003 = (usuario1002, usuario1003)

usuariosTestEJ10B = [usuario1000, usuario1001]
usuariosTestEJ10C = [usuario1000, usuario1002]
usuariosTestEJ10D = [usuario1001, usuario1002]
usuariosTestEJ10E = [usuario1000, usuario1001, usuario1002]
usuariosTestEJ10F = usuariosTestEJ10E
usuariosTestEJ10G = [usuario1000, usuario1001, usuario1002, usuario1003]
usuariosTestEJ10H = usuariosTestEJ10G

relacionesTestEJ10C = [relacion1000_1002]
relacionesTestEJ10D = [relacion1001_1002]
relacionesTestEJ10E = [relacion1000_1001]
relacionesTestEJ10F = [relacion1001_1002]
relacionesTestEJ10G = [relacion1000_1001, relacion1002_1003]
relacionesTestEJ10H = [relacion1000_1001, relacion1001_1002, relacion1002_1003]

redTestEJ10A = ([],[],[])
redTestEJ10B = (usuariosTestEJ10B, [], [])
redTestEJ10C = (usuariosTestEJ10C, relacionesTestEJ10C, [])
redTestEJ10D = (usuariosTestEJ10D, relacionesTestEJ10D, [])
redTestEJ10E = (usuariosTestEJ10E, relacionesTestEJ10E, [])
redTestEJ10F = (usuariosTestEJ10E, relacionesTestEJ10F, [])
redTestEJ10G = (usuariosTestEJ10G, relacionesTestEJ10G, [])
redTestEJ10H = (usuariosTestEJ10H, relacionesTestEJ10H, [])

usuarioGenericoA = (1, "A") -- Uitilizado como parametro reduntante
usuarioGenericoB = (2, "B") -- Uitilizado como parametro reduntante

-- Casos Permutables:
-- Los casos permutables se testean debido a que en los casos anteriores la lista
-- de usuarios siempre se pasó de manera ordenada donde el primer usuario de la lista era el
-- primer elemento de la secuencia, y el último elemento de la lista era el último de la secuencia.
usuariosTestEJ10I_P1 = [usuario1000, usuario1001, usuario1002]
usuariosTestEJ10I_P2 = [usuario1001, usuario1000, usuario1002]
usuariosTestEJ10I_P3 = [usuario1001, usuario1002, usuario1000]
usuariosTestEJ10I_P4 = [usuario1000, usuario1002, usuario1001]
usuariosTestEJ10I_P5 = [usuario1002, usuario1000, usuario1001]
usuariosTestEJ10I_P6 = [usuario1002, usuario1001, usuario1000]

relacionesTestEJ10I = [relacion1000_1001, relacion1001_1002]

redTestEJ10I_P1 = (usuariosTestEJ10I_P1, relacionesTestEJ10I, [])
redTestEJ10I_P2 = (usuariosTestEJ10I_P2, relacionesTestEJ10I, [])
redTestEJ10I_P3 = (usuariosTestEJ10I_P3, relacionesTestEJ10I, [])
redTestEJ10I_P4 = (usuariosTestEJ10I_P4, relacionesTestEJ10I, [])
redTestEJ10I_P5 = (usuariosTestEJ10I_P5, relacionesTestEJ10I, [])
redTestEJ10I_P6 = (usuariosTestEJ10I_P6, relacionesTestEJ10I, [])