
--Objetivo 1
-- Definición del tipo de datos para representar una persona
data Persona = Persona {
    nombre :: String
} deriving(Show, Eq)

--Objetivo 1.1(?)
amigos = [Persona "Juan", Persona "Pedro", Persona "Santiago"]

 
--Objetivo 2
-- Definición del tipo de datos para representar un gasto
data Gasto = Gasto {
    pagador :: Persona,
    montoTotal :: Int
} deriving(Show, Eq)

-- -- Definición del tipo de datos para representar una deuda
data Deuda = Deuda {
    deudor :: Persona,   --Persona/s que debe pagar la deuda
    acreedor :: Persona, --Persona a la cual hay que pagarle la deuda
    montoDeuda :: Int
} deriving(Show, Eq)

--Objetivo 2.1
-- Funcion
calcularDeudas amigos (Gasto pagador montoTotal) =
    let montoPersona = montoTotal / fromIntegral (length amigos)