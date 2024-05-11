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


instance Eq Deuda where
    (Deuda d1 a1 m1) == (Deuda d2 a2 m2) =
        d1 == d2 && a1 == a2 && m1 == m2



--FUNCIONES

--Recibe un grupo de personas y un gasto y crea deudas en base a ese gasto
crearDeudasDeGasto :: [Persona] -> Gasto -> [Deuda]
crearDeudasDeGasto amigos (Gasto pagador monto) =
    let montoPorPersona = monto / fromIntegral (length amigos)
        deudores = filter (\p -> p /= pagador) amigos
    in map (\deudor -> Deuda deudor pagador montoPorPersona) deudores


-- Recibe una lista de deudas y las empareja. Ej: [deuda1,deuda2,deuda3] -> [(deuda1,deuda2),(deuda2,deuda3),(deuda1,deuda3)]
emparejarDeudas :: (Eq a) => [a] -> [(a, a)]
emparejarDeudas [] = []
emparejarDeudas (x:xs) = emparejarDeuda x xs ++ emparejarDeudas xs
    where
        emparejarDeuda _ [] = []
        emparejarDeuda y (z:zs) = (y, z) : emparejarDeuda y zs


-- Recibe una tupla de deudas y devuelve 1 o 0 deudas, dependiendo de los montos
gestionarMontosDeDeudas :: (Deuda, Deuda) -> [Deuda]
gestionarMontosDeDeudas ((Deuda deudor1 acreedor1 montoDeuda1), (Deuda deudor2 acreedor2 montoDeuda2)) =
    if montoDeuda1 >= montoDeuda2
        then [Deuda deudor1 acreedor1 (montoDeuda1 - montoDeuda2)]
        else [Deuda deudor2 acreedor2 (montoDeuda2 - montoDeuda1)]


-- Recibe una lista de tuplas de deudas y devuelve una lista con las deudas saldadas donde corresponda
saldarDeudas :: (Deuda, Deuda) -> [Deuda]
saldarDeudas (deuda1, deuda2) =
    if (deudor deuda1 == acreedor deuda2) && (acreedor deuda1 == deudor deuda2)
        then gestionarMontosDeDeudas (deuda1, deuda2)
        else [deuda1, deuda2] 


-- Recibe una lista de deudas y devuelve esa lista de deudas actualizada
actualizarDeudas :: [Deuda] -> [Deuda]
actualizarDeudas deudas = 
    let deudasEmparejadas = emparejarDeudas deudas
    in concatMap saldarDeudas deudasEmparejadas  


-- Recibe una lista de deudas y las muestra por pantalla
imprimirDeudas :: [Deuda] -> IO ()
imprimirDeudas [] = return ()
imprimirDeudas ((Deuda deudor acreedor montoDeuda): restoDeudas) = do
    if montoDeuda > 0 
        then putStrLn $ nombre deudor ++ " le debe $" ++ show montoDeuda ++ " a " ++ nombre acreedor 
        else return ()
    imprimirDeudas restoDeudas 

{- 
imprimirDeudasEmparejadas :: [(Deuda, Deuda)] -> IO ()
imprimirDeudasEmparejadas [] = return ()
imprimirDeudasEmparejadas (((Deuda deudor1 acreedor1 montoDeuda1),(Deuda deudor2 acreedor2 montoDeuda2)): restoDeudasEmparejadas) = do
        putStrLn $ "tupla:\n" ++ nombre deudor1 ++ " le debe $" ++ show montoDeuda1 ++ " a " ++ nombre acreedor1 ++ "\n" ++ 
                   nombre deudor2 ++ " le debe $" ++ show montoDeuda2 ++ " a " ++ nombre acreedor2  
        imprimirDeudasEmparejadas restoDeudasEmparejadas -}



-- Ejemplo de uso
main :: IO ()
main = do
    let juan = Persona "Juan" 
        pedro = Persona "Pedro" 
        santiago = Persona "Santiago"
        amigos = [juan, pedro, santiago]
        gasto1 = Gasto juan 60
        gasto2 = Gasto pedro 90
        deudas = crearDeudasDeGasto amigos gasto1
        nuevasDeudas = crearDeudasDeGasto amigos gasto2
        todasDeudas = deudas ++ nuevasDeudas 
        deudasActualizadas = actualizarDeudas todasDeudas
    imprimirDeudas deudasActualizadas
    