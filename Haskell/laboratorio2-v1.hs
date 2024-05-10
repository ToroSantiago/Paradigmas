--Objetivo 1
-- Definición del tipo de datos para representar una persona
--{-# LANGUAGE BlockArguments #-}
data Persona = Persona {
    nombre :: String
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
    deudores :: [Persona],   --Persona/s que debe pagar la deuda
    acreedor :: Persona, --Persona a la cual hay que pagarle la deuda
    montoDeuda :: Float
} deriving(Show, Eq)

type Deudas = [Deuda]

-- Funciones
calcularDeudas amigos (Gasto pagador montoTotal) =
    montoTotal / fromIntegral (length amigos)

obtenerAcreedor (Gasto pagador montoTotal) = pagador

obtenerDeudores (persona : restoAmigos) (Gasto pagador montoTotal) =
    if persona == pagador
        then restoAmigos
        else persona : obtenerDeudores restoAmigos (Gasto pagador montoTotal)

--------------------------------------------------------------------------------------------------------------------------------------------
--Objetivo 3

actualizarDeudas amigos gasto (deuda : restoDeuda) =
    let pagadorGasto = pagador gasto
        montoPorAmigo = calcularDeudas amigos gasto
        in 
        if acreedor deuda == pagadorGasto                       --si el acreedor de la deuda que estoy analizando es quien esta pagando este nuevo gasto
            then deuda { montoDeuda = montoDeuda deuda + montoPorAmigo }     --la deuda de los demas amigos aumenta
            else if pagadorGasto `elem` deudores deuda                       --en cambio, si el pagador estaba debiendo en esta deuda
                then deuda { montoDeuda = montoDeuda deuda - montoPorAmigo } --su deuda disminuye, porque ya la estaria saldando con este gasto
                else deuda                                                   --en cualquier otro caso, la deuda se mantiene igual 
    --in map actualizarDeuda deudas

main :: IO ()
main = do
    let juan = Persona "Juan"
        pedro = Persona "Pedro"
        santiago = Persona "Santiago"
        amigos = [juan, pedro, santiago]
        gasto1 = Gasto juan 60
        deuda1 = Deuda {deudores = obtenerDeudores amigos gasto1, acreedor = obtenerAcreedor gasto1, montoDeuda = calcularDeudas amigos gasto1}
        gasto2 = Gasto pedro 90
        deuda2 = Deuda {deudores = obtenerDeudores amigos gasto2, acreedor = obtenerAcreedor gasto2, montoDeuda = calcularDeudas amigos gasto2}
        deudas = [deuda1, deuda2]
        deudasActualizadas = actualizarDeudas amigos gasto2 deudas
    print deudasActualizadas