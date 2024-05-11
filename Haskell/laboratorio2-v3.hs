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
crearDeudasDeGasto :: [Persona] -> Gasto -> [Deuda]
crearDeudasDeGasto amigos (Gasto pagador monto) =
    let montoPorPersona = monto / fromIntegral (length amigos)
        deudores = filter (\p -> p /= pagador) amigos
    in map (\deudor -> Deuda deudor pagador montoPorPersona) deudores


emparejarDeudas :: (Eq a) => [a] -> [(a, a)]
emparejarDeudas [] = []
emparejarDeudas (x:xs) = emparejarDeuda x xs ++ emparejarDeudas xs
    where
        emparejarDeuda _ [] = []
        emparejarDeuda y (z:zs) = (y, z) : emparejarDeuda y zs


saldarDeudas [] = []
saldarDeudas [(deuda1, deuda2) : restoDeudasEmparejadas] = 
    let deuda1 = if deudor deuda1 == acreedor deuda2 
                            then (\deuda -> saldarMonto (montoDeuda deuda1) (montoDeuda deuda2))
                            else (\deuda -> actualizarDeuda deuda (+) montoPorDeudor)
        nuevoMontoDeuda = map (\deuda -> gestionarDeuda deuda) (deudas persona)
    in
    deuda1 { montoDeuda = nuevoMontoDeuda } : saldarDeudas restoDeudasEmparejadas 


--actualizarDeudas [] = []
actualizarDeudas deudas =
    let deudasEmparejadas = map emparejarDeudas deudas
    in map saldarDeudas deudasEmparejadas


imprimirDeudas [] = return ()
imprimirDeudas ((Deuda deudor acreedor montoDeuda): restoDeudas) = do
    if montoDeuda > 0 
        then putStrLn $ show (nombre deudor) ++ " le debe $" ++ show montoDeuda ++ " a " ++ nombre acreedor 
        else imprimirDeudas restoDeudas

-- Ejemplo de uso
main :: IO ()
main = do
    let juan = Persona "Juan" 
        pedro = Persona "Pedro" 
        santiago = Persona "Santiago"
        amigos = [juan, pedro, santiago]
        gasto1 = Gasto juan 60
        gasto2 = Gasto pedro 90
    
    {-putStrLn "Amigos:"
    print amigos
    
    putStrLn "Gastos realizados:"
    print gasto1
    print gasto2 -}
    
        deudas = crearDeudasDeGasto amigos gasto1
        nuevasDeudas = crearDeudasDeGasto amigos gasto2
        todasDeudas = deudas ++ nuevasDeudas 
    putStrLn "Deudas generadas:"
    imprimirDeudas todasDeudas