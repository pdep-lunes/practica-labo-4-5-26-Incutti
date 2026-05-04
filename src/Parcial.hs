module Parcial where
import Text.Show.Functions()

-- PARA QUE NO SE ROMPAN LOS TESTS CUANDO PRUEBO SI COMPILA
-- QUEDA HASTA HACER TESTS REALES
doble :: Number -> Number
doble num = num * 2

type Juguete = String
type RazasExtravagantes = [String]
listaRazaExtravagante :: RazasExtravagantes
listaRazaExtravagante = ["dalmata", "pomerania"]

data Perrito = UnPerrito{
                        raza :: String,
                        juguetesFavoritos :: [Juguete],
                        tiempoEnGuarderia :: Int,
                        energia :: Number
} deriving (Show, Eq)

data Actividad = UnaActividad{
                        ejercicio :: (Perrito -> Perrito),
                        tiempo :: Int
} deriving (Show, Eq)

data Guarderia = UnaGuarderia{
                        nombre :: String,
                        rutina :: [Actividad]
} deriving (Show, Eq)

-- PARTE A
jugar :: Perrito -> Perrito
jugar unPerrito = unPerrito { energia = energia unPerrito - 10 } 
-- corregir para no considerar negativos

ladrar :: Int -> Perrito -> Perrito
ladrar cantidadLadridos unPerrito = unPerrito { energia = energia unPerrito + (cantidadLadridos/2) }

regalar :: Juguete -> Perrito -> Perrito
regalar unJuguete unPerrito = unPerrito { juguetesFavoritos = (:) unJuguete (juguetesFavoritos unPerrito) }

esRazaExtravagante :: Perrito -> Bool
esRazaExtravagante unPerrito = elem (raza unPerrito) listaRazaExtravagante
-- este esta bien? no me acuerdo si tenia q pasar la lista por params 
leQuedaTiempo :: Int -> Perrito -> Bool
leQuedaTiempo tiempoRestante unPerrito = tiempoEnGuarderia unPerrito >= tiempoRestante
tieneAccesoASpa :: Perrito -> Bool
tieneAccesoASpa unPerrito = leQuedaTiempo 50 unPerrito || esRazaExtravagante
energiaA100 :: Perrito -> Perrito
energiaA100 unPerrito = unPerrito { energia = 100 }
regalarPeine :: Perrito -> Perrito
regalarPeine unPerrito = regalar "peine de goma" unPerrito
diaDeSpa :: Perrito -> Perrito
diaDeSpa unPerrito
    | tieneAccesoASpa unPerrito = energiaA100 . regalarPeine $ unPerrito
    | otherwise unPerrito = unPerrito

perderPrimerJuguete :: Perrito -> Perrito
perderPrimerJuguete unPerrito = unPerrito { juguetesFavoritos = drop 1 . juguetesFavoritos $ unPerrito }
diaDeCampo :: Perrito -> Perrito
diaDeCampo unPerrito = jugar . perderPrimerJuguete $ unPerrito


zara = UnPerrito "dalmata" ["pelota", "mantita"] 90 80
guarderiaDePerritos = UnaGuarderia "guarderia de perritos", [UnaActividad jugar 30, 
                                                             UnaActividad (ladrar 18) 20, 
                                                             UnaActividad (regalar "pelota") 0, 
                                                             UnaActividad diaDeSpa 120, 
                                                             UnaActividad diaDeCampo 720]

-- PARTE B
duracionRutina :: Guarderia -> Int
duracionRutina unaGuarderia = sum . map tiempo . rutina $ unaGuarderia   
perritoDisponible :: Perrito -> Guarderia -> Bool
perritoDisponible unPerrito unaGuarderia = tiempoEnGuarderia unPerrito >= duracionRutina unaGuarderia

perritoResponsable :: Perrito -> Bool
perritoResponsable unPerrito = 3 > length . juguetesFavoritos . diaDeCampo $ unPerrito
