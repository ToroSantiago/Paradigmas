-- Definición del tipo de datos para representar una persona
data Persona = Persona {
    nombre :: String
} deriving (Show, Eq)

-- Definición del tipo de datos para representar un gasto
data Gasto = Gasto {
    pagador :: Persona,
    monto :: Float
} deriving (Show)

-- Definición del tipo de datos para representar una deuda
data Deuda = Deuda {
    deudor :: Persona,
    acreedor :: Persona,
    montoDeuda :: Float
} deriving (Show)

-- Función para calcular las deudas después de un gasto
crearDeudas :: [Persona] -> Gasto -> [Deuda]
crearDeudas amigos (Gasto pagador monto) =
    let montoPorPersona = monto / fromIntegral (length amigos)
        deudores = filter (\p -> p /= pagador) amigos
    in map (\deudor -> Deuda deudor pagador montoPorPersona) deudores



{- listarDeudas [] = []
listarDeudas (deuda : restoDeudas) =
    deuda 
    listarDeudas restoDeudas -}

-- Ejemplo de uso
main :: IO ()
main = do
    let juan = Persona "Juan" 
        pedro = Persona "Pedro" 
        santiago = Persona "Santiago"
        amigos = [juan, pedro, santiago]
        gasto1 = Gasto juan 60
        gasto2 = Gasto pedro 90
    
    putStrLn "Amigos:"
    print amigos
    
    putStrLn "Gastos realizados:"
    print gasto1
    print gasto2
    
    let deudas = crearDeudas amigos gasto1
        nuevasDeudas = crearDeudas amigos gasto2
        todasDeudas = deudas ++ nuevasDeudas 
    putStrLn "Deudas generadas:"
    print todasDeudas