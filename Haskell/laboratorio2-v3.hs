import qualified Data.Set as Set

-- Definición del tipo de datos para representar una persona
data Persona = Persona {
    nombre :: String
} deriving (Show, Eq, Ord)


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
} deriving (Show,Eq,Ord)


-- FUNCIONES

-- Recibe un grupo de personas y un gasto y crea deudas en base a ese gasto
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
gestionarMontosDeDeudas (deuda1, deuda2) =
    if montoDeuda deuda1 >= montoDeuda deuda2
        then [Deuda (deudor deuda1) (acreedor deuda1)  ((montoDeuda deuda1) - (montoDeuda deuda2))]
        else [Deuda (deudor deuda2) (acreedor deuda2)  ((montoDeuda deuda2) - (montoDeuda deuda1))]
        --then [deuda1 { montoDeuda = montoDeuda deuda1 - montoDeuda deuda2 }]
        --else [deuda2 { montoDeuda = montoDeuda deuda2 - montoDeuda deuda1 }]


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


-- Recibe dos lista de deudas y elimina de la segunda todos los elementos de la primera
eliminarDeudas :: [Deuda] -> [Deuda] -> [Deuda]
eliminarDeudas [] deudas = deudas
eliminarDeudas (x:xs) deudas = eliminarDeudas xs (filter (\d -> d /= x) deudas)


obtenerDeudasModificadas [] deudas = []
obtenerDeudasModificadas (x:xs) deudasActualizadas = 
    obtenerDeudasModificadas xs (filter (\d -> (acreedor d /= acreedor x)&&(acreedor d /= deudor x)) deudasActualizadas)


-- Recibe una lista de deudas y las muestra por pantalla
{- imprimirDeudas :: [Deuda] -> IO ()
imprimirDeudas [] = return ()
imprimirDeudas ((Deuda deudor acreedor montoDeuda): restoDeudas) = do
    if montoDeuda > 0 
        then putStrLn $ nombre deudor ++ " le debe $" ++ show montoDeuda ++ " a " ++ nombre acreedor 
        else return ()
    imprimirDeudas restoDeudas  -}

-- Recibe una lista de deudas y una lista de deudas sin duplicados
imprimirDeudas :: [Deuda] -> IO ()
imprimirDeudas deudas = do
    let deudasSet = Set.fromList deudas -- Convertir la lista de deudas a un conjunto para eliminar duplicados
    imprimirDeudasSet (Set.toList deudasSet) -- Convertir el conjunto nuevamente a una lista y llamar a la función auxiliar


-- Recibe una lista de deudas y las muestra por pantalla
imprimirDeudasSet :: [Deuda] -> IO ()
imprimirDeudasSet [] = return ()
imprimirDeudasSet ((Deuda deudor acreedor montoDeuda): restoDeudas) = do
    if montoDeuda > 0 
        then putStrLn $ nombre deudor ++ " le debe $" ++ show montoDeuda ++ " a " ++ nombre acreedor 
        else return ()
    imprimirDeudasSet restoDeudas

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
        deuda1 = crearDeudasDeGasto amigos gasto1
        deuda2 = crearDeudasDeGasto amigos gasto2
        deudasCompletas = deuda1 ++ deuda2
        deudasActualizadas = actualizarDeudas deudasCompletas
        deudasDiferentes = eliminarDeudas deudasCompletas deudasActualizadas
        deudasModificadas = obtenerDeudasModificadas deudasDiferentes deudasActualizadas
        deudasSinModificar = eliminarDeudas deudasModificadas deudasActualizadas
        deudasNuevas = deudasSinModificar ++ deudasDiferentes

    putStrLn "\nDeudas completas:"
    imprimirDeudas deudasCompletas

    putStrLn "\nDeudas actualizadas:"
    imprimirDeudas deudasActualizadas

    putStrLn "\nDeudas diferentes:"
    imprimirDeudas deudasModificadas

    putStrLn "\nDeudas modificadas:"
    imprimirDeudas deudasModificadas

    putStrLn "\nDeudas sin modificar:"
    imprimirDeudas deudasSinModificar

    --putStrLn "\nDeudas nuevas:"
    --imprimirDeudas deudasNuevas
    