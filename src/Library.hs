module Library where
import PdePreludat

data Gimnasta = UnGimnasta {
    nombre :: String
,   edad :: Number
,   peso :: Number
,   coefTonificacion :: Number
}deriving (Show,Eq)

pancho = UnGimnasta {
    nombre = "Francisco"
,   edad = 40
,   peso = 120
,   coefTonificacion = 1
}

andres = UnGimnasta {
    nombre = "Andy"
,   edad = 22
,   peso = 80
,   coefTonificacion = 6
}

-- 1)

estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not(estaObeso gimnasta)) && (coefTonificacionMayorA 5 gimnasta)

estaObeso :: Gimnasta -> Bool
estaObeso = (>100).peso

coefTonificacionMayorA :: Number -> Gimnasta -> Bool
coefTonificacionMayorA coeficiente gimnasta = (coefTonificacion gimnasta) > coeficiente

-- 2)

type Calorias = Number

quemarCalorias :: Calorias -> Gimnasta -> Gimnasta
quemarCalorias calorias gimnasta
    | estaObeso gimnasta = bajarPeso (1/150 * calorias) gimnasta
    | (not (estaObeso gimnasta)) && (tieneMasDeNAños 30 gimnasta) && (calorias > 200) = bajarPeso 1 gimnasta
    | otherwise = bajarPeso (calorias/((peso gimnasta)*(edad gimnasta))) gimnasta

bajarPeso :: Number -> Gimnasta -> Gimnasta
bajarPeso pesoBajado gimnasta = gimnasta {peso = peso gimnasta - pesoBajado}

tieneMasDeNAños :: Number -> Gimnasta -> Bool
tieneMasDeNAños años gimnasta = (edad gimnasta) > años

-- 3) 

-- a)
type Ejercicio = Gimnasta -> Gimnasta
type Minutos = Number
type Velocidad = Number
type Kilos = Number
type Inclinicacion = Number

-- i)
caminataEnCinta :: Minutos -> Ejercicio
caminataEnCinta minutosDeEntrenamiento = quemarCalorias (5*minutosDeEntrenamiento)

-- ii)
entrenamientoEnCinta :: Minutos -> Ejercicio
entrenamientoEnCinta = quemarCalorias.promedioVelocidadAlcanzada

promedioVelocidadAlcanzada :: Minutos -> Velocidad
promedioVelocidadAlcanzada =  (/2).(+6).(/5)

-- b) 
pesas :: Kilos -> Minutos -> Ejercicio
pesas kilosALevantar minutosDeEntrenamiento
    | minutosDeEntrenamiento > 10 = tonificarGimnasta kilosALevantar
    | otherwise = id

tonificarGimnasta :: Kilos -> Gimnasta -> Gimnasta
tonificarGimnasta kilosALevantar gimnasta = gimnasta {coefTonificacion = coefTonificacion gimnasta + (kilosALevantar/10)}

-- c)
colina :: Inclinicacion -> Minutos -> Ejercicio
colina inclinacion minutosDeEntrenamiento = quemarCalorias (caloriasAQuemarColina inclinacion minutosDeEntrenamiento)

caloriasAQuemarColina :: Inclinicacion -> Minutos -> Number
caloriasAQuemarColina inclinacion = (*2).(*inclinacion)

-- d)
montaña :: Inclinicacion -> Minutos -> Ejercicio
montaña inclinacion minutosDeEntrenamiento = (colinaIncrementada inclinacion (minutosDeEntrenamiento/2)).(colina inclinacion (minutosDeEntrenamiento/2))

colinaIncrementada :: Inclinicacion -> Minutos -> Gimnasta -> Gimnasta
colinaIncrementada inclinacion minutosDeEntrenamiento = colina (inclinacion +3) minutosDeEntrenamiento

-- 4)

-- a)
data Rutina = UnaRutina {
    nombreRutina :: String
,   duracion :: Number
,   ejercicios :: [Ejercicio]
}deriving (Show,Eq)

-- i)
everest = UnaRutina {
    nombreRutina = "Everest"
,   duracion = 40
,   ejercicios = [colina 3 (duracion everest), pesas 10 (duracion everest), caminataEnCinta (duracion everest), montaña 3 (duracion everest)]
}

infiernodiabetico = UnaRutina {
    nombreRutina = "Infierno Diabetico"
,   duracion = 666
,   ejercicios = [colina 20 120, entrenamientoEnCinta 60, pesas 60 120]
}

-- ii) recursividad

hacerRutina :: Rutina -> Gimnasta -> Gimnasta
hacerRutina rutina gimnasta = hacerEjerciciosRutina gimnasta (ejercicios rutina)

hacerEjerciciosRutina :: Gimnasta -> [Ejercicio] -> Gimnasta
hacerEjerciciosRutina gimnasta [] = gimnasta -- Paso base
hacerEjerciciosRutina gimnasta (ej1:ejercicios) = hacerEjerciciosRutina (hacerEjercicio gimnasta ej1) ejercicios -- Paso recursivo

hacerEjercicio :: Gimnasta -> Ejercicio -> Gimnasta
hacerEjercicio gimnasta ejercicio = ejercicio gimnasta

-- iii) foldl
hacerRutina2 :: Gimnasta -> Rutina -> Gimnasta
hacerRutina2 gimnasta rutina = foldl hacerEjercicio gimnasta (ejercicios rutina)

-- b)
hacerResumenRutina :: Gimnasta -> Rutina -> (String,Number,Number)
hacerResumenRutina gimnasta rutina = (nombreRutina rutina,kilosPerdidos gimnasta rutina, tonificacionGanada gimnasta rutina)

kilosPerdidos :: Gimnasta -> Rutina -> Number
kilosPerdidos gimnasta rutina = (peso gimnasta) - (peso (hacerRutina rutina gimnasta))

tonificacionGanada :: Gimnasta -> Rutina -> Number
tonificacionGanada gimnasta rutina = (coefTonificacion (hacerRutina rutina gimnasta)) - (coefTonificacion gimnasta)

-- 5)

listarutinas = [everest,infiernodiabetico]

rutinasSaludables :: [Rutina] -> Gimnasta -> [Rutina]
rutinasSaludables listaRutinas gimnasta = filter (esRutinaSaludable gimnasta) listaRutinas

esRutinaSaludable :: Gimnasta -> Rutina -> Bool
esRutinaSaludable gimnasta rutina = estaSaludable (hacerRutina2 gimnasta rutina)



