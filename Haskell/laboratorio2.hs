import qualified Data.Set as Set

-- Definici贸n del tipo de datos para representar una persona
data Persona = Persona {
    nombre :: String
} deriving (Show, Eq, Ord)


-- Definici贸n del tipo de datos para representar un gasto
data Gasto = Gasto {
    pagador :: Persona,
    monto :: Float
} deriving (Show)


-- Definici贸n del tipo de datos para representar una deuda
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


-- Objetivo 2
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


obtenerDeudasModificadas :: [Deuda] -> [Deuda] -> [Deuda]
obtenerDeudasModificadas deudas1 deudas2 = filter (\deuda2 -> (any (coincideDeuda deuda2) deudas1)) deudas2
  where
    coincideDeuda :: Deuda -> Deuda -> Bool
    coincideDeuda (Deuda deudor1 acreedor1 _) (Deuda deudor2 acreedor2 _) =
        (deudor1 == deudor2 || deudor1 == acreedor2) && (acreedor1 == deudor2 || acreedor1 == acreedor2)


-- Objetivo 4
-- Función para calcular el balance de una persona dentro del grupo
consultarBalance :: Persona -> [Gasto] -> [Persona] -> Float
consultarBalance persona gastos amigos =
    let deudas = concatMap (crearDeudasDeGasto amigos) gastos
        deudasPersona = filter (\deuda -> deudor deuda == persona || acreedor deuda == persona) deudas
        montoTotalAcreedor = sum [montoDeuda deuda | deuda <- deudasPersona, acreedor deuda == persona]
        montoTotalDeudor = sum [montoDeuda deuda | deuda <- deudasPersona, deudor deuda == persona]
    in (montoTotalAcreedor - montoTotalDeudor)


-- Función para listar el balance de todos los integrantes del grupo
listarBalances :: [Gasto] -> [Persona] -> [(Persona, Float)]
listarBalances gastos amigos = [(persona, consultarBalance persona gastos amigos) | persona <- amigos]


-- Recibe una lista de deudas y una lista de deudas sin duplicados
imprimirDeudas :: [Deuda] -> IO ()
imprimirDeudas deudas = do
    let deudasSet = Set.fromList deudas
    imprimirDeudasSet (Set.toList deudasSet)


-- Recibe una lista de deudas y las muestra por pantalla
imprimirDeudasSet :: [Deuda] -> IO ()
imprimirDeudasSet [] = return ()
imprimirDeudasSet ((Deuda deudor acreedor montoDeuda): restoDeudas) = do
    if montoDeuda > 0 
        then putStrLn $ nombre deudor ++ " le debe $" ++ show montoDeuda ++ " a " ++ nombre acreedor 
        else return ()
    imprimirDeudasSet restoDeudas


-- Ejemplo de uso
main :: IO ()
main = do
    let juan = Persona "Juan" 
        pedro = Persona "Pedro" 
        santiago = Persona "Santiago"
        amigos = [juan, pedro, santiago]
        --Objetivo 2
        gasto1 = Gasto juan 60
        gasto2 = Gasto pedro 90
        deudas1 = crearDeudasDeGasto amigos gasto1
        deudas2 = crearDeudasDeGasto amigos gasto2
        deudasCompletas = deudas1 ++ deudas2
        -- Objetivo 3
        deudasActualizadas = actualizarDeudas deudasCompletas
        deudasDiferentes = eliminarDeudas deudasCompletas deudasActualizadas
        deudasModificadas = obtenerDeudasModificadas deudasDiferentes deudasActualizadas
        deudasSinModificar = eliminarDeudas deudasModificadas deudasActualizadas
        deudasNuevas = deudasSinModificar ++ deudasDiferentes
        -- Objetivo 4
        deudasBalanceadas = listarBalances [gasto1, gasto2] amigos

    putStrLn "\nDeudas completas:"
    imprimirDeudas deudasCompletas

    putStrLn "\nDeudas actualizadas:"
    imprimirDeudas deudasActualizadas

    putStrLn "\nDeudas diferentes:"
    imprimirDeudas deudasDiferentes

    putStrLn "\nDeudas modificadas:"
    imprimirDeudas deudasModificadas

    putStrLn "\nDeudas sin modificar:"
    imprimirDeudas deudasSinModificar

    putStrLn "\nDeudas nuevas:"
    imprimirDeudas deudasNuevas

    putStrLn "\nBalance de deudas:"
    print deudasBalanceadas