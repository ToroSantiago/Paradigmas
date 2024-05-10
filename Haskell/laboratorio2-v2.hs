--Objetivo 1
data Persona = Persona {
    nombre :: String,
    deudas :: [Deuda]
} deriving(Show, Eq)

--------------------------------------------------------------------------------------------------------------------------------------------
--Objetivo 2
data Gasto = Gasto {
    pagador :: Persona,
    montoTotal :: Float
} deriving(Show, Eq)

data Deuda = Deuda {
    acreedor :: Persona,
    montoDeuda :: Float
} deriving(Show, Eq)


-- Funciones

imprimirDeudas :: [Persona] -> IO ()
imprimirDeudas [] = return ()
imprimirDeudas (persona : restoAmigos) = do
    mapM_ (\deuda -> putStrLn $ show (nombre persona) ++ " le debe $" ++ show (montoDeuda deuda) ++ " a " ++ nombre (acreedor deuda)) (deudas persona)
    imprimirDeudas restoAmigos

{- imprimirDeudas [persona : restoAmigos] = do
    map (\deuda -> putStrLn $ show persona ++ " le debe $" ++ show (montoDeuda deuda) ++ " a " ++ nombre (acreedor deuda) ) (deudas persona)
 -}
agregarDeuda persona deuda = persona { deudas = deudas persona ++ [deuda] }

calcularDeudas :: Foldable t => t a -> Gasto -> Float
calcularDeudas amigos (Gasto pagador montoTotal) =
    montoTotal / fromIntegral (length amigos)

obtenerAcreedor (Gasto pagador montoTotal) = pagador

actualizarDeuda deuda funcion monto =
    deuda { montoDeuda = funcion (montoDeuda deuda) monto }


--------------------------------------------------------------------------------------------------------------------------------------------
--Objetivo 3
actualizarDeudasDePersonas [] _ _ = []
actualizarDeudasDePersonas (persona : restoPersonas) (Gasto pagador montoTotal) cantidadPersonas =
    let montoPorDeudor = montoTotal / fromIntegral cantidadPersonas
        gestionarDeuda = if pagador == persona
                            then (\deuda -> actualizarDeuda deuda (-) montoPorDeudor)
                            else (\deuda -> actualizarDeuda deuda (+) montoPorDeudor)
        nuevasDeudas = map (\deuda -> gestionarDeuda deuda) (deudas persona)
    in
    persona { deudas = nuevasDeudas } : actualizarDeudasDePersonas restoPersonas (Gasto pagador montoTotal) cantidadPersonas


main :: IO ()
main = do
    let juan = Persona "Juan" []
        pedro = Persona "Pedro" []
        santiago = Persona "Santiago" []
        amigos = [juan, pedro, santiago]
        gasto1 = Gasto juan 60
        deuda1 = Deuda {acreedor = obtenerAcreedor gasto1, montoDeuda = calcularDeudas amigos gasto1}
        gasto2 = Gasto pedro 90
        deuda2 = Deuda {acreedor = obtenerAcreedor gasto2, montoDeuda = calcularDeudas amigos gasto2}
        juanConDeudas1 = agregarDeuda juan deuda2
        pedroConDeudas1 = agregarDeuda pedro deuda1
        santiagoConDeudas1 = agregarDeuda santiago deuda1
        santiagoConDeudas2 = agregarDeuda santiagoConDeudas1 deuda2
        amigosConDeudas = [juanConDeudas1, pedroConDeudas1, santiagoConDeudas2]
        amigosConDeudasActualizadas = actualizarDeudasDePersonas amigosConDeudas gasto2 (length amigosConDeudas)
    
    putStrLn "Deudas:"
    imprimirDeudas amigosConDeudasActualizadas
    --putStrLn "Amigos sin deudas:"
    --putStrLn ""
    --print amigos

    --putStrLn "Primer gasto:"
    --print gasto1

    --putStrLn "Primera deuda:"
    --print deuda1

    --putStrLn "Amigos con deudas"
    --print amigosConDeudas

    --putStrLn "Segundo gasto:"
    --print gasto2

    --putStrLn "Segunda deuda:"
    --print deuda2

    --putStrLn "Deudas actualizadas con el segundo gasto:"
    --print amigosConDeudasActualizadas
