--Objetivo 1
-- Definición del tipo de datos para representar una persona
--{-# LANGUAGE BlockArguments #-}
data Persona = Persona {
    nombre :: String,
    deudas :: [Deuda]
} deriving(Show, Eq)

type Amigos = [Persona]

--------------------------------------------------------------------------------------------------------------------------------------------
--Objetivo 2
-- Definición del tipo de datos para representar un gasto
data Gasto = Gasto {
    pagador :: Persona,
    montoTotal :: Float
} deriving(Show, Eq)

type Gastos = [Gasto]

-- Definición del tipo de datos para representar una deuda
data Deuda = Deuda {
    acreedor :: Persona,
    montoDeuda :: Float
} deriving(Show, Eq)

type Deudas = [Deuda]

-- Funciones

agregarDeuda persona deuda = persona { deudas = deudas persona ++ [deuda] }


{- actualizarDeudas (deuda : restoDeudas) funcion monto = 
    deuda { montoDeuda = montoDeuda deuda funcion monto}
    actualizarDeudas restoDeudas funcion monto -}
        
--actualizarDeudas :: Deudas -> (Float -> Float -> Float) -> Float -> Deudas
actualizarDeudas [] _ _ = []
actualizarDeudas (deuda : restoDeudas) funcion monto =
    deuda { montoDeuda = funcion (montoDeuda deuda) monto } : actualizarDeudas restoDeudas funcion monto

actualizarDeudasDePersonas [] _ _ = []
actualizarDeudasDePersonas (persona : restoPersonas) (Gasto pagador montoTotal) cantidadPersonas =
    let montoPorDeudor = montoTotal / fromIntegral cantidadPersonas
        actualizarDeuda = if pagador == persona
                            then (\deuda -> actualizarDeudas deuda) (- montoPorDeudor)
                            else (\deuda -> actualizarDeudas deuda) (+ montoPorDeudor)
        nuevasDeudas = map (\deuda -> actualizarDeuda deuda) (deudas persona)
    in
    persona { deudas = nuevasDeudas } : actualizarDeudasDePersonas restoPersonas (Gasto pagador montoTotal) cantidadPersonas


--------------------------------------------------------------------------------------------------------------------------------------------
--Objetivo 3

main :: IO ()
main = do
    let juan = Persona "Juan" []
        pedro = Persona "Pedro" []
        santiago = Persona "Santiago" []
        amigos = [juan, pedro, santiago]
        gasto1 = Gasto juan 60
        gasto2 = Gasto pedro 90
        deuda1 = actualizarDeudasDePersonas amigos gasto1 (length amigos) 
        deuda2 = actualizarDeudasDePersonas amigos gasto2 (length amigos)
    print deuda2