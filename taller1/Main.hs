import Util
import Data.Maybe
import Data.List

-- Ejercicio 1
singleton:: Eq t => t -> Anillo t
singleton x = A x (\t -> if (t == x) then Just x else Nothing)

insertar :: Eq t => t -> Anillo t -> Anillo t
insertar x (A actual f) = A actual g
    where g e   | (e == actual) = Just x
                | (e == x) = f actual
                | otherwise = f e

avanzar :: Anillo t -> Anillo t
avanzar (A actual f) = A ((fromJust.f) actual) f

-- Ejercicio 2

enAnillo:: Eq t => t -> Anillo t -> Bool
enAnillo e a = 
    elem e (anilloALista a) 

-- Ejercicio 3
filterAnillo :: Eq t => (t -> Bool) -> Anillo t -> Maybe (Anillo t)
filterAnillo p a = listaAAnillo (filter p (anilloALista a))

-- Ejercicio 4

mapAnillo:: Eq a => Eq b => (a -> b) -> Anillo a -> Anillo b
mapAnillo f a = fromJust(listaAAnillo (map f (anilloALista(a))))

--Ejercicio 5
palabraFormable :: String -> [Anillo Char] -> Bool
palabraFormable s as = all (\(x, y) -> y `enAnillo` x) (zip as s)

--Ejercicio 6
anillos:: Eq a => [a] -> [Anillo a]
anillos xs = map (fromJust.listaAAnillo) (filter (not.null) (concat (map permutaciones (partes xs)))) 

--Auxiliares

anilloALista :: Eq t => Anillo t -> [t] 
anilloALista a = 
    (primero : (takeWhile (/= primero) (iterate iterador segundo)))
    where
        primero = actual a
        segundo =  actual (avanzar a)
        iterador = fromJust.(siguiente a)

listaAAnillo :: Eq t => [t] -> Maybe(Anillo t)
listaAAnillo [] = Nothing
listaAAnillo (x:xs) = 
    Just( 
        foldr
            (\e r -> insertar e r)
            (singleton x)
            xs
        )

partes :: [a] -> [[a]]
partes = foldr (\x res -> res ++ (map (x:) res)) [[]]


partir :: [ a ] -> [ ( [ a ], [ a ] ) ]
partir xs = [ ( take i xs, drop i xs ) | i <- [ 0.. ( length xs ) ] ]

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec-> concatMap (agregarEnTodasLasPosiciones x) rec) [[]] 
    where agregarEnTodasLasPosiciones j js = [ (fst h)++[j]++(snd h)| h <- (partir js)]