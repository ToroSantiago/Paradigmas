data Persona = Persona {
    nombre :: String
} deriving(Show, Eq)

amigos = [Persona]

--persona1 =  Persona {nombre = "Juan"}
--persona2 =  Persona {nombre = "Pedro"}
--persona3 =  Persona {nombre = "Santiago"}



{-tuplas =  [("juan", 10), ("pedro", 20)]
sumaTuplas :: [(String, Integer)] -> Integer
 sumaTuplas [] = 0
 sumaTuplas ((nombre, cantidad) : resto) = 
     cantidad + sumaTuplas resto
sumaTuplas = sum . (map extraerCantidad)

extraerCantidades :: [(String, Integer)] -> [Integer]   
extraerCantidades []  = []
extraerCantidades ((nombre, cantidad) : resto) =
    (cantidad : extraerCantidades resto)

extraerCantidad :: (String, Integer) -> Integer
extraerCantidad (nombre, cantidad) = cantidad -}

-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b