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

redVacia = ([], [], [])
testEj1 = test [
    "Caso 1: La red no tiene usuarios" ~: nombresDeUsuarios redVacia  ~?= [],
  --"Caso 2: La red tiene mas de un usuario" ~: nombresDeUsuarios redA = ["Juan","Natalia","Pedro","Mariela"], -> Testeado en main
    "Caso 3: La red tiene usuarios con nombres repetidos" ~: nombresDeUsuarios redB ~?= ["Juan", "Pedro", "Natalia"]
 ]

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

-- Casos Permutadbles:
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

testEj10 = test [
    "Caso 1: La red no tiene usuarios"                ~: existeSecuenciaDeAmigos redTestEJ10A usuarioGenericoA usuarioGenericoB ~?= False,
    "Caso 2: La red no tiene relaciones"              ~: existeSecuenciaDeAmigos redTestEJ10B usuarioGenericoA usuarioGenericoB ~?= False,
    "Caso 3: 1001 no esta en la red"                  ~: existeSecuenciaDeAmigos redTestEJ10C usuario1000 usuario1001 ~?= False,
    "Caso 4: 1000 no esta en la red"                  ~: existeSecuenciaDeAmigos redTestEJ10D usuario1000 usuario1001 ~?= False,
    "Caso 5: 1002 no tiene amigos"                    ~: existeSecuenciaDeAmigos redTestEJ10E usuario1000 usuario1002 ~?= False,
    "Caso 6: 1000 no tiene amigos"                    ~: existeSecuenciaDeAmigos redTestEJ10F usuario1000 usuario1002 ~?= False,
    "Caso 7: No existe la secuencia de amigos"        ~: existeSecuenciaDeAmigos redTestEJ10G usuario1000 usuario1003 ~?= False,
    "Caso 8: Existe la secuencia de amigos"           ~: existeSecuenciaDeAmigos redTestEJ10H usuario1000 usuario1003 ~?= True,
    "Case permutado 1: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P1 usuario1000 usuario1002 ~?= True,
    "Case permutado 2: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P2 usuario1000 usuario1002 ~?= True,
    "Case permutado 3: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P3 usuario1000 usuario1002 ~?= True,
    "Case permutado 4: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P4 usuario1000 usuario1002 ~?= True,
    "Case permutado 5: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P5 usuario1000 usuario1002 ~?= True,
    "Case permutado 6: Existe la secuencia de amigos" ~: existeSecuenciaDeAmigos redTestEJ10I_P6 usuario1000 usuario1002 ~?= True
    ]
