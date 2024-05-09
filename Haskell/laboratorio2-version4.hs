-- Definición del tipo de datos para representar una persona
data Persona = Persona {
    nombre :: String
} deriving (Show, Eq)

-- Definición del tipo de datos para representar un gasto
data Gasto = Gasto {
    pagador :: Persona,
    beneficiarios :: [Persona],
    monto :: Float
} deriving (Show)

-- Definición del tipo de datos para representar una deuda
data Deuda = Deuda {
    deudor :: Persona,
    acreedor :: Persona,
    montoDeuda :: Float
} deriving (Show)

-- Función para calcular las deudas después de un gasto
calcularDeudas :: [Persona] -> [Gasto] -> [Deuda]
calcularDeudas amigos gastos = concatMap (calcularDeudasPorGasto amigos) gastos

-- Función auxiliar para calcular las deudas después de un gasto específico
calcularDeudasPorGasto :: [Persona] -> Gasto -> [Deuda]
calcularDeudasPorGasto amigos (Gasto pagador beneficiarios monto) =
    let montoPorPersona = monto / fromIntegral (length beneficiarios)
        deudas = filter (\p -> p /= pagador) amigos
        deudasActualizadas = map (\deudor -> Deuda deudor pagador (montoPorPersona + montoDeudaPorPersona deudor pagador amigos)) deudas
    in deudasActualizadas

-- Función auxiliar para calcular las deudas de una persona con todas las demás
calcularDeudasDePersona :: [Persona] -> Persona -> [Deuda]
calcularDeudasDePersona amigos persona =
    let deudas = filter (\p -> p /= persona) amigos
    in map (\deudor -> Deuda deudor persona 0) deudas

-- Función auxiliar para obtener la deuda entre dos personas
montoDeudaPorPersona :: Persona -> Persona -> [Persona] -> Float
montoDeudaPorPersona deudor acreedor amigos =
    case find (\deuda -> deudor == deudaDeudor deuda && acreedor == deudaAcreedor deuda) (calcularTodasLasDeudas amigos) of
        Just deuda -> montoDeuda deuda
        Nothing -> 0

-- Función para calcular todas las deudas entre los amigos
calcularTodasLasDeudas :: [Persona] -> [Deuda]
calcularTodasLasDeudas amigos = concatMap (calcularDeudasDePersona amigos) amigos

-- Función para consultar el balance de una persona dentro del grupo
consultarBalance :: Persona -> [Gasto] -> [Persona] -> Float
consultarBalance persona gastos amigos =
    let deudas = calcularDeudas amigos gastos
        deudasAcreedor = filter (\deuda -> persona == acreedor deuda) deudas
        deudasDeudor = filter (\deuda -> persona == deudor deuda) deudas
        totalAcreedor = sum $ map montoDeuda deudasAcreedor
        totalDeudor = sum $ map montoDeuda deudasDeudor
    in totalAcreedor - totalDeudor
-- Objetivo 4: Implementar la función consultarBalance que recibe una persona, una lista de gastos y una lista de las personas que participan en el grupo y retorne el balance de esa persona dentro del grupo.

-- Función para listar el balance de todos los integrantes del grupo
listarBalances :: [Gasto] -> [Persona] -> [(Persona, Float)]
listarBalances gastos amigos = map (\persona -> (persona, consultarBalance persona gastos amigos)) amigos

-- Ejemplo de uso
main :: IO ()
main = do
    let amigos = [Persona "Juan", Persona "Pedro", Persona "Santiago"]
    let gastos = [Gasto (Persona "Juan") [Persona "Pedro", Persona "Santiago"] 60, Gasto (Persona "Pedro") [Persona "Juan"] 30, Gasto (Persona "Pedro") [Persona "Santiago"] 90]
    
    putStrLn "Amigos:"
    print amigos
    
    putStrLn "Gastos realizados:"
    print gastos

    let balances = listarBalances gastos amigos
    putStrLn "Balances:"
    print balances