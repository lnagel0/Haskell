-- EJERCICIO 1
-- Uso esta funcion para sumar los goles en total
sumarLista :: [Int] -> Int
sumarLista [x] = x
sumarLista (x:xs) = x + sumarLista xs

atajaronSuplentes :: [(String, String)] -> [Int] -> Int -> Int
atajaronSuplentes [(a, b)] [c] d | c == d = 0
atajaronSuplentes (x:xs) (y:ys) a | length (x:xs) == length (y:ys) && 
                                    equiposValidos (x:xs) && 
                                    length (y:ys) > 0 &&
                                    sumarLista (y:ys) <= a = a - sumarLista (y:ys)


-- EJERCICIO 2 
-- Verifica si una dupla particular equipo-jugador pertenece o no a la lista de equipo-jugador
perteneceEquipo :: (String, String) -> [(String, String)] -> Bool
perteneceEquipo (a, b) [(c, d)] | a == c || a == d || b == c || b == d = True
                                | otherwise = False
perteneceEquipo (a, b) (x:xs) | a == fst x || a == snd x || b == fst x || b == snd x = True
                              | otherwise = perteneceEquipo (a, b) xs

equiposValidos :: [(String, String)] -> Bool 
equiposValidos [(a, b)] | a /= b = True
                        | otherwise = False
equiposValidos (x:xs) | fst x == snd x || perteneceEquipo x xs = False
                      | otherwise = equiposValidos xs


--EJERCICIO 3
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

-- Chequea si un arquero es titular de un equipo
esArquero :: String -> [(String, String)] -> Bool
esArquero a [(b, c)] | a == c = True
                     | otherwise = False
esArquero a (x:xs) | a /= snd x = esArquero a xs
                   | otherwise = True

-- Devuelve la cantidad de goles de X arquero
golesArquero :: String -> [(String, String)] -> [Int] -> Int
golesArquero a (x:xs) (y:ys) | a == snd x = y
                             | otherwise = golesArquero a xs ys

porcentajeDeGoles :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeGoles a (x:xs) (y:ys) | esArquero a (x:xs) && 
                                    equiposValidos (x:xs) && 
                                    length (x:xs) == length (y:ys) && 
                                    sumarLista (y:ys) > 0 = 
                                    (division (golesArquero a (x:xs) (y:ys)) (sumarLista (y:ys)))*100


-- EJERCICIO 4
vallaMenosVencida :: [(String, String)] -> [Int] -> String
vallaMenosVencida [(a, b)] [c] = b
vallaMenosVencida (x:c:xs) (y:z:ys) | equiposValidos (x:c:xs) && 
                                      length (x:c:xs) == length (y:z:ys) && 
                                      sumarLista (y:z:ys) > 0 &&
                                      length (y:z:ys) > 0 &&
                                      y <= z = vallaMenosVencida (x:xs) (y:ys)
                                    | otherwise = vallaMenosVencida (c:xs) (z:ys)
