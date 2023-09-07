fizzBuzz :: Int -> String
fizzBuzz x | x == 0 = "Fin"
fizzBuzz x | mod x 5 == 0 && mod x 3 == 0 = "FizzBuzziano"
fizzBuzz x | mod x 3 == 0 = "Fizz"
fizzBuzz x | mod x 5 == 0 = "Buzz"
           | otherwise = show x

--Guia 3 ejercicio 2
absoluto :: Int -> Int
absoluto 0 = 0
absoluto x | x < 0 = x * (-1)
absoluto x = x

maximoAbs :: Int -> Int -> Int
maximoAbs x y | absoluto x >= absoluto y = absoluto x
maximoAbs x y | absoluto y >= absoluto x = absoluto y

parcialF :: Int -> Int
parcialF 1 = 8
parcialF 4 = 131
parcialF 16 = 16

parcialG :: Int -> Int
parcialG 8 = 16
parcialG 16 = 4
parcialG 131 = 1

parcialFoG :: Int -> Int
parcialFoG x = parcialF (parcialG x)

parcialGoF :: Int -> Int
parcialGoF x = parcialG (parcialF x)

maximoTres :: Int -> Int -> Int -> Int
maximoTres x y z | x >= y && x >= z = x
maximoTres x y z | y >= x && y >= z = y
maximoTres x y z | z >= x && z >= y = z

digitoDecenas :: Int -> Int
digitoDecenas x = div x 10

todoMenor :: (Int, Int) -> (Int, Int) -> Bool
todoMenor (a, b) (c, d) | a < c && b < d = True
todoMenor (a, b) (c, d)  = False

posPar :: (Int, Int, Int) -> Int
posPar (x, y, z) | even x = 1
posPar (x, y, z) | even y = 2
posPar (x, y, z) | even z = 3
posPar (x, y, z) = 4

algunoEsCero :: Float -> Float -> String
algunoEsCero 0 y = "El primer numero es 0"
algunoEsCero x 0 = "El segundo numero es 0"
algunoEsCero x y = "Ninguno es 0"

ambosSonCero :: Float -> Float -> String
ambosSonCero x y | x == 0 && y == 0 = "Ambos son 0"
ambosSonCero x y = "Ambos numeros no son 0"

mismoIntervalo :: (Int, Int) -> String
mismoIntervalo (x, y) | x <= 3 && y <= 3 = "Pertenecen al mismo intervalo menor que 3"
mismoIntervalo (x, y) | (x > 3 && x <= 7) && (y > 3 && x <= 7) = "Pertenecen al mismo intervalo entre 3 y 7"
mismoIntervalo (x, y) | x > 7 && y > 7 = "Pertenecen al mismo intervalo mayor que 7"
mismoIntervalo (x, y) = "No pertenecen al mismo intervalo"

esMultiplo :: Int -> Int -> String
esMultiplo x y | mod y x == 0 = "El primer numero es multiplo del segundo"
esMultiplo x y = "El primer numero no es multiplo del segundo"

--Ejercicio 4
prodInt :: (Int, Int) -> (Int, Int) -> Int
prodInt (a, b) (c, d) = (a * c) + (b * d)

distPuntos :: (Float, Float) -> (Float, Float) -> Float
distPuntos (a, b) (c, d) = sqrt ((a - c)**2 + (b - d)**2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

--Guia 4
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

parteEntera :: Float -> Int
parteEntera = truncate

esDivisible :: Int -> Int -> Bool
esDivisible a b | a < b = False
esDivisible a b | a == b = True
esDivisible a b = esDivisible (a-b) b

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares x = ((x*2) - 1) + sumaImpares (x - 1)

medFact :: Int -> Int
medFact 0 = 1
medFact 1 = 1
medFact x = x * medFact (x-2)

main :: IO ()
main =
    print ()