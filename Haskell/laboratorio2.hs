--Objetivo 1
-- Definición del tipo de datos para representar una persona
--{-# LANGUAGE BlockArguments #-}
{-data Persona = Persona {
    nombre :: String
} deriving(Show, Eq)-}

--Objetivo 1.1
--------------------------- CODIGO DE PRUEBA ---------------------------
--persona1 = Persona {nombre = "Juan"}
--persona2 = Persona {nombre = "Pedro"}
--persona3 = Persona {nombre = "Santiago"}
--amigos = [persona1, persona2, persona3]

--------------------------------------------------------------------------------------------------------- 
--Objetivo 2
-- Definición del tipo de datos para representar un gasto
data Gasto = Gasto {
    pagador :: Persona,
    montoTotal :: Float
} deriving(Show, Eq)

-- -- Definición del tipo de datos para representar una deuda
data Deuda = Deuda {
    --deudor :: Persona,   --Persona/s que debe pagar la deuda
    acreedor :: Persona, --Persona a la cual hay que pagarle la deuda
    montoDeuda :: Float
} deriving(Show, Eq)

data Persona = Persona {
    nombre :: String,
    deudas :: [Deuda]   --al principio va a ser lista vacia, de esta forma se evita la recursion infinita xd
} deriving(Show, Eq)

-- Funcion
calcularDeudas amigos (Gasto pagador montoTotal) =
    montoTotal / fromIntegral (length amigos)

obtenerAcreedor (Gasto pagador montoTotal) = pagador

obtenerDeudores (persona : restoAmigos) (Gasto pagador montoTotal) =
    if persona == pagador
        then restoAmigos
        else persona : obtenerDeudores restoAmigos (Gasto pagador montoTotal)


--Objetivo 2.1
--------------------------- CODIGO DE PRUEBA ---------------------------
--gasto1 = Gasto {pagador = persona1, montoTotal = 60}

--------------------------------------------------------------------------------------------------------- 
--Objetivo 3
--listarDeudas (persona : restoAmigos) = 
--    persona 


--deuda1 = Deuda {acreedor = obtenerAcreedor gasto1, montoDeuda = calcularDeudas amigos gasto1}
--persona1 . deudas ++ deuda1 