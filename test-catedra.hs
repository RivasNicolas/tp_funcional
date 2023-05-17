import Test.HUnit
import iap1-tp
main = runTestTT tests

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

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
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
testEj9 = test[
    "Caso 1: la red no tiene publicaciones"                                                    ~: tieneUnSeguidorFiel redTEST_EJ9A usuario900 ~?= False,
    "Caso 2: la red tiene menos de 2 usuarios diferentes"                                      ~: tieneUnSeguidorFiel redTEST_EJ9B usuario900 ~?= False,
    "Caso 3: el usuario no tiene publicaciones en esta red"                                    ~: tieneUnSeguidorFiel redTEST_EJ9C usuario903 ~?= False,
    "Caso 4: no existe un usuario u2 al que le gusten TODAS las publicaciones de u1"           ~: tieneUnSeguidorFiel redTEST_EJ9D usuario900 ~?= False,
    "Caso 5: existe por lo menos un usuario u2 al que le gustan TODAS las publicaciones de u1" ~: tieneUnSeguidorFiel redTEST_EJ9E usuario900 ~?= True
 ]

usuario1000 = (1000, "A")
usuario1001 = (1001, "B")
usuario1002 = (1002, "C")
usuario1003 = (1003, "D")
usuario1004 = (1004, "E")
usuario1005 = (1005, "F") -- Este usuario debe estar relacionados con todos (excepto 1006)
usuario1006 = (1006, "G") -- Este usuario no debe estar relacionado con ningun otro

relacion1000_1001 = (usuario1000, usuario1001)
relacion1001_1002 = (usuario1001, usuario1002)
relacion1002_1003 = (usuario1002, usuario1003)
relacion1003_1004 = (usuario1003, usuario1004)

relacion1005_1000 = (usuario1005, usuario1000)
relacion1005_1001 = (usuario1005, usuario1001)
relacion1005_1002 = (usuario1005, usuario1002)

relacionesTestEJ10A = [relacion1000_1001]
usuariosTestEJ10A = [usuario1000, usuario1001]
redTestEJ10A = (usuariosTestEJ10A, relacionesTestEJ10A, [])

relacionesTestEJ10B = [relacion1000_1001, relacion1001_1002]
usuariosTestEJ10B = [usuario1000, usuario1001, usuario1002]
redTestEJ10B = (usuariosTestEJ10B, relacionesTestEJ10B, [])

relacionesTestEJ10C = [relacion1000_1001, relacion1001_1002, relacion1002_1003, relacion1003_1004]
usuariosTestEJ10C_Per1 = [usuario1000, usuario1001, usuario1002, usuario1003, usuario1004]
usuariosTestEJ10C_Per2 = [usuario1000, usuario1001, usuario1003, usuario1002, usuario1004]
usuariosTestEJ10C_Per3 = [usuario1000, usuario1002, usuario1001, usuario1003, usuario1004]
usuariosTestEJ10C_Per4 = [usuario1000, usuario1002, usuario1003, usuario1001, usuario1004]
usuariosTestEJ10C_Per5 = [usuario1000, usuario1003, usuario1001, usuario1002, usuario1004]
usuariosTestEJ10C_Per6 = [usuario1000, usuario1003, usuario1002, usuario1001, usuario1004]
redTestEJ10C = (usuariosTestEJ10C_Per1, relacionesTestEJ10C, [])

relacionesTestEJ10D = [relacion1000_1001, relacion1001_1002, relacion1002_1003, relacion1003_1004]
usuariosTestEJ10D = [usuario1000, usuario1001, usuario1002, usuario1006, usuario1003, usuario1004]
redTestEJ10D = (usuariosTestEJ10D, relacionesTestEJ10D, [])

relacionesTestEJ10E = [relacion1000_1001, relacion1001_1002, relacion1005_1000, relacion1005_1001, relacion1005_1002]
usuariosTestEJ10E_Per1A = [usuario1000, usuario1001, usuario1002, usuario1005]
usuariosTestEJ10E_Per1B = [usuario1000, usuario1002, usuario1001, usuario1005]
usuariosTestEJ10E_Per2A = [usuario1005, usuario1001, usuario1002, usuario1000]
usuariosTestEJ10E_Per2B = [usuario1005, usuario1002, usuario1001, usuario1000]
redTestEJ10E = (usuariosTestEJ10E_Per1A, relacionesTestEJ10E, [])

-- Para testear este ejercicio los elementos de los extremos del
-- parametro [Usuarios] no se deben permutar (ya que eso nunca puede ocurrir)
-- por ejemplo si testeamos [u1, u2, u3, u4], tambien debemos testear [u1, u3, u2, u4]
-- si fuera [u1, u2, u3, u4, u5] testeamos [u1, (permutaciones de [u2,u3,u4]), u5]
testCadenaDeAmigos = runTestTT testEJ10A
testEJ10A = test [
    " cadenaDeAmigos 2U, 1R V" ~: (cadenaDeAmigos2 usuariosTestEJ10A redTestEJ10A) ~?= True,
    " cadenaDeAmigos 3U, 2R V" ~: (cadenaDeAmigos2 usuariosTestEJ10B redTestEJ10B) ~?= True,
    " cadenaDeAmigos 5U, 4R P1 V" ~: (cadenaDeAmigos2 usuariosTestEJ10C_Per1 redTestEJ10C) ~?= True,
    " cadenaDeAmigos 5U, 4R P2 V" ~: (cadenaDeAmigos2 usuariosTestEJ10C_Per2 redTestEJ10C) ~?= True,
    " cadenaDeAmigos 5U, 4R P3 V" ~: (cadenaDeAmigos2 usuariosTestEJ10C_Per3 redTestEJ10C) ~?= True,
    " cadenaDeAmigos 5U, 4R P4 V" ~: (cadenaDeAmigos2 usuariosTestEJ10C_Per4 redTestEJ10C) ~?= True,
    " cadenaDeAmigos 5U, 4R P5 V" ~: (cadenaDeAmigos2 usuariosTestEJ10C_Per5 redTestEJ10C) ~?= True,
    " cadenaDeAmigos 5U, 4R P6 V" ~: (cadenaDeAmigos2 usuariosTestEJ10C_Per6 redTestEJ10C) ~?= True,
    " un usuario no tiene ninguna relacion F" ~: (cadenaDeAmigos2 usuariosTestEJ10D redTestEJ10D) ~?= False,
    " un usuario tiene a todos en su lista de amigos A V" ~: (cadenaDeAmigos2 usuariosTestEJ10E_Per1A redTestEJ10E) ~?= True,
    " un usuario tiene a todos en su lista de amigos B V" ~: (cadenaDeAmigos2 usuariosTestEJ10E_Per1B redTestEJ10E) ~?= True,
    " un usuario tiene a todos en su lista de amigos C V" ~: (cadenaDeAmigos2 usuariosTestEJ10E_Per2A redTestEJ10E) ~?= True,
    " un usuario tiene a todos en su lista de amigos D V" ~: (cadenaDeAmigos2 usuariosTestEJ10E_Per2B redTestEJ10E) ~?= True
 ]
