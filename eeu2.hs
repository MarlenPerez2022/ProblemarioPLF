--Pérez Gómez Marlen Citlalli-------------
-------------------------Funciones Básicas------------------------

-- Ejercicio 1. Definir la función promedio3 tal que (promedio3 x y z) es
-- la promedio aritmética de los números x, y y z. Por ejemplo,
-- promedio3 1 3 8 == 4.0
-- promedio3 (-1) 0 7 == 2.0
-- promedio3 (-3) 0 3 == 0.0
promedio3 :: Floating a => a -> a -> a -> a
promedio3 x y z = (x + y + z) / 3



-- Ejercicio 2. Definir la función sumaMonedas tal que
-- (sumaMonedas a b c d e) es la suma de las monedas  correspondientes a
-- a monedas de 1 peso, b de 2 pesos, c de 5 pesos, d 10 pesos y
-- e de 20 pesos. Por ejemplo,
--sumaMonedas 0 0 0 0 1 == 20
--sumaMonedas 0 0 8 0 3 == 100
--sumaMonedas 1 1 1 1 1 == 38
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = ((a * 1) + (b * 2) + (c * 5) + (d * 10) + (e * 20))

-- -----------------------------------


-- Ejercicio 3. Definir la función volumenEsfera tal que
-- (volumenEsfera r) es el volumen de la esfera de radio r. Por ejemplo,
-- volumenEsfera 10 == 4188.790204786391
-- Indicación: Usar la constante pi.
volumenEsfera :: Floating a => a -> a
volumenEsfera r = ((4 / 3) * (pi) * (r^3))
-- -----------------------------------

-- Ejercicio 4. Definir la función areaDeCoronaCircular tal que
-- (areaDeCoronaCircular r1 r2) es el área de una corona circular de
-- radio interior r1 y radio exterior r2. Por ejemplo,
--areaDeCoronaCircular 1 2 == 9.42477796076938
--areaDeCoronaCircular 2 5 == 65.97344572538566
--areaDeCoronaCircular 3 5 == 50.26548245743669
areaDeCoronaCircular :: Floating a => a -> a -> a
areaDeCoronaCircular r1 r2 = ((pi) * ((r2^2) - (r1^2)))


-- Ejercicio 5. Definir la función ultimaCifra tal que (ultimaCifra x)
-- es la última cifra del número x. Por ejemplo,
-- ultimaCifra 325 == 5
-- Indicación: Usar la función rem
ultimaCifra :: Int -> Int
ultimaCifra x = x `rem` 10

-- Ejercicio 6. Definir la función maxTres tal que (maxTres x y z) es
-- el máximo de x, y y z. Por ejemplo,
-- maxTres 6 2 4 == 6
-- maxTres 6 7 4 == 7
-- maxTres 6 7 9 == 9
-- Indicación: Usar la función max.
maxTres :: Ord a => a -> a -> a -> a
maxTres x y z = max x (max y z)


-- Ejercicio 7. Definir la función rota1 tal que (rota1 xs) es la lista
-- obtenida poniendo el primer elemento de xs al final de la lista. Por
-- ejemplo,
-- rota1 [3,2,5,7] == [2,5,7,3]
-- Indicación: Usar la función tail head
-- -----------------------------------
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]


-- Ejercicio 8. Definir la función rota tal que (rota n xs) es la lista
-- obtenida poniendo los n primeros elementos de xs al final de la
-- lista. Por ejemplo,
-- rota 1 [3,2,5,7] == [2,5,7,3]
-- rota 2 [3,2,5,7] == [5,7,3,2]
-- rota 3 [3,2,5,7] == [7,3,2,5]
-- Indicación: Usar la función drop take
rota :: Int -> [a] -> [a]
rota y xs = drop y xs ++ take y xs


-- Ejercicio 9. Definir la función rango tal que (rango xs) es la
-- lista formada por el menor y mayor elemento de xs.
-- rango [3,2,7,5] == [2,7]
-- Indicación: Usar minimum y maximum
rango :: Ord a => [a] -> [a]
rango xs = [minimum xs, maximum xs]


-- Ejercicio 10. Definir la función palindromo tal que (palindromo xs) se
-- verifica si xs es un palíndromo; es decir, es lo mismo leer xs de
-- izquierda a derecha que de derecha a izquierda. Por ejemplo,
-- palindromo [3,2,5,2,3] == True
-- palindromo [3,2,5,6,2,3] == False
-- Indicación: utiliza la función reverse
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs


-- Ejercicio 11. Definir la función interior tal que (interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs. Por ejemplo,
-- interior [2,5,3,7,3] == [5,3,7]
-- interior [2..7] == [3,4,5,6]
-- Indicación : utiliza la función tail init
interior :: [a] -> [a]
interior xs = tail (init xs)


-- Ejercicio 13. Definir la función segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y
-- n. Por ejemplo,
-- segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
-- segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
-- segmento 5 3 [3,4,1,2,7,9,0] == []
-- Indicación: utiliza la función drop take
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = take (n - m + 1) (drop m xs)


-- Ejercicio 14. Definir la función extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs. Por ejemplo,
-- extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
-- Indicación: utiliza la función take drop
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs


-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función mediano tal que (mediano x y z) es el
-- número mediano de los tres números x, y y z. Por ejemplo,
-- mediano 3 2 5 == 3
-- mediano 2 4 5 == 4
-- mediano 2 6 5 == 5
-- mediano 2 6 6 == 6
-- Indicación: Usar maximum y minimum.
mediano :: Ord a => a -> a -> a -> a
mediano x y z = x + y + z - maximum [x,y,z] - minimum [x,y,z]


-- Ejercicio 16. Definir la función tresIguales tal que
-- (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales. Por ejemplo,
-- tresIguales 4 4 4 == True
-- tresIguales 4 3 4 == False
tresIguales :: Eq a => a -> a -> a -> Bool
tresIguales x y z = (x == z) && (y == z)


-- Ejercicio 17. Definir la función tresDiferentes tal que
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son
-- distintos. Por ejemplo,
-- tresDiferentes 3 5 2 == True
-- tresDiferentes 3 5 3 == False
tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes x y z = (x /= z) && (y /= z) && (x /= z)


-- Ejercicio 18. Definir la función cuatroIguales tal que
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son
-- iguales. Por ejemplo,
-- cuatroIguales 5 5 5 5 == True
-- cuatroIguales 5 5 4 5 == False
-- Indicación: Usar la función tresIguales.
cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z u = (x == y) && (y == z) && (z == u)



------------------------- Guardas y Patrones ------------------------
-- Ejercicio 1. Definir la función
-- divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
-- divisionSegura 7 2 == 3.5
-- divisionSegura 7 0 == 9999.0
divisionSegura :: Double -> Double -> Double
divisionSegura x y
  | y == 0    = 9999.0   
  | otherwise = x / y    



-- Ejercicio 2 La disyunción excluyente xor de dos fórmulas se
-- verifica si una es verdadera y la otra es falsa. Su tabla de verdad
-- es
--x     | y     | xor x y
-------------------------
--True  | True  | False
--True  | False | True
--False | True  | True
--False | False | False
--
-- Definir la función
--xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad.

xor1 :: Bool -> Bool -> Bool
xor1 True False = True
xor1 False True = True
xor1 _ _ = False
-- ---------------------------------------------------------------------
-- Ejercicio 3. Las dimensiones de los rectángulos puede representarse
-- por pares; por ejemplo, (5,3) representa a un rectángulo de base 5 y
-- altura 3.
--
-- Definir la función
--mayorRectangulo :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integ
-- tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre
-- r1 y r2. Por ejemplo,
--mayorRectangulo (4,6) (3,7) == (4,6)
--mayorRectangulo (4,6) (3,8) == (4,6)
--mayorRectangulo (4,6) (3,9) == (3,9)
mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo r1@(b1, h1) r2@(b2, h2)
  | b1 * h1 >= b2 * h2 = r1
  | otherwise = r2

-- -------------------------------------------------

-- Ejercicio 4. Definir la función
-- intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p) es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo,
-- intercambia (2,5) == (5,2)
-- intercambia (5,2) == (2,5)
   intercambia :: (a, b) -> (b, a)
   intercambia (x, y) = (y, x)

-- Ejercicio 5. Definir la función
-- distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo,
-- distancia (1,2) (4,6) == 5.0
    distancia :: (Double, Double) -> (Double, Double) -> Double
    distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Ejercicio 6. Definir una función
-- ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista. Por ejemplo,
-- ciclo [2,5,7,9] == [9,2,5,7]
-- ciclo [] == []
-- ciclo [2] == [2]
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs

-- ----------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
-- numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,
-- numeroMayor 2 5 == 52
-- numeroMayor 5 2 == 52
numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y
  | x >= y = 10 * x + y
  | otherwise = 10 * y + x

-- ---------------------------------------------------------------------

-- Ejercicio 8. Definir la función
-- numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raíces reales de la
-- ecuación a*x^2 + b*x + c = 0. Por ejemplo,
-- numeroDeRaices 2 0 3 == 0
-- numeroDeRaices 4 4 1 == 1
-- numeroDeRaices 5 23 12 == 2
numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
  | d> 0 = 2 
  | d == 0 = 1
  | otherwise = 0
  where d = b^2 - 4 * a * c
-- ------------------------------------

-- Ejercicio 9. Definir la función
-- raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0. Por ejemplo,
-- raices 1 3 2 == [-1.0,-2.0]
-- raices 1 (-2) 1 == [1.0,1.0]
-- raices 1 0 1 == []
  raices :: Double -> Double -> Double -> [Double]
  raices a b c
  | a > 0 = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
  | d== 0 = [(-b) / (2 * a)]
  | otherwise = []
  where d = b^2 - 4 * a * c
-- -----------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 10. En geometría, la fórmula de Herón, descubierta por
-- Herón de Alejandría, dice que el área de un triángulo cuyo lados
-- miden a, b y c es la raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperímetro
-- s = (a+b+c)/2
--
-- Definir la función
-- area :: Double -> Double -> Double -> Double
-- tal que (area a b c) es el área del triángulo de lados a, b y c. Por
-- ejemplo,
-- area 3 4 5 == 6.0
area :: Double -> Double -> Double -> Double
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2
-- ----------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 11. Los intervalos cerrados se pueden representar mediante
-- una lista de dos números (el primero es el extremo inferior del
-- intervalo y el segundo el superior).
--
-- Definir la función
--interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2. Por ejemplo,
--interseccion [] [3,5]== []
--interseccion [3,5] []== []
--interseccion [2,4] [6,9] == []
--interseccion [2,6] [6,9] == [6,6]
--interseccion [2,6] [0,9] == [2,6]
--interseccion [2,6] [0,4] == [2,4]
--interseccion [4,6] [0,4] == [4,4]
--interseccion [5,6] [0,4] == []
  interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1, b1] [a2, b2]
  | max a1 a2 <= min b1 b2 = [max a1 a2, min b1 b2]
  | otherwise = []
-- -------------------------------------

-- Ejercicio 12. Los triángulos aritméticos se forman como sigue
--1
--2 3
--4 5 6
--7 8 9 10
--11 12 13 14 15
--16 17 18 19 20 21
--
-- Definir la función
--linea :: Integer -> [Integer]
-- tal que (linea n) es la línea n-ésima de los triángulos
-- aritméticos. Por ejemplo,
--linea 4 == [7,8,9,10]
--linea 5 == [11,12,13,14,15]
linea :: Integer -> [Integer]
linea n = [inicio .. fin]
  where
    inicio = (n * (n - 1)) `div` 2 + 1
    fin = inicio + n - 1

---------------------------Recursividad -----------------------

-- Ejercicio 1. Definir por recursión la función
--potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
--potencia 2 3 == 8
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)



-- Ejercicio 2. Dados dos números naturales, a y b, es posible
-- calcular su máximo común divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
--mcd(a,b) = a, si b = 0
--         = mcd (b, a módulo b), si b > 0
--
-- Definir la función
--mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)
-- ---------------------------------------------------------------------
-- Ejercicio 3, Definir por recursión la función
--pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo,
--pertenece 3 [2,3,5] == True
--pertenece 4 [2,3,5] == False
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
  | x == y    = True
  | otherwise = pertenece x ys
-- ------------------------------------

-- Ejercicio 4. Definir por recursión la función
--tomar :: Int -> [a] -> [a]
-- tal que (tomar n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo,
--tomar 3 [4..12] => [4,5,6]
tomar :: Int -> [a] -> [a]
tomar 0 _      = []
tomar _ []     = []
tomar n (x:xs) = x : tomar (n - 1) xs
-- ---------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir, por comprensión, la función
--digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo,
--digitosC 320274 == [3,2,0,2,7,4]
digitosC :: Integer -> [Integer]
digitosC 0 = []
digitosC n = [read [c] | c <- show n]
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir, por recursión, la función
-- sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
-- sumaDigitosR 3 == 3
-- sumaDigitosR 2454 == 15
-- sumaDigitosR 20045 == 11
-- -----------------------------
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = n `mod` 10 + sumaDigitosR (n `div` 10)
--se repite hasta que el número se reduzca a 0


-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación rápida se selecciona el primer elemento x de xs, se divide
-- los restantes en los menores o iguales que x y en los mayores que x,
-- se ordena cada una de las dos partes y se unen los resultados. Por
-- ejemplo, para ordenar la lista [3,1,4,1,5,9,2] el proceso es el
-- siguiente:
-- or [3,1,4,1,5,9,2]
-- = or [1,1,2] ++ [3] ++ or [4,5,9]
-- = (or [1] ++ [1] ++ or [2]) ++ [3] ++ (or [] ++ [4] ++ or [5,9])
-- = ((or [] ++ [1] ++ or []) ++ [1] ++ (or [] ++ [2] ++ or []))
-- ++ [3] ++ ([] ++ [4] ++ (or [] ++ [5] ++ or [9]))
-- = (([] ++ [1] ++ []) ++ [1] ++ ([] ++ [2] ++ []))
-- ++ [3] ++ ([4] ++ ([] ++ [5] ++ (or [] ++ [9] ++ or [])))
-- = ([1] ++ [1] ++ [2] ++
-- ++ [3] ++ ([4] ++ ([5] ++ (or [] ++ [9] ++ or [])))
-- = ([1] ++ [1] ++ [2] ++
-- ++ [3] ++ ([4] ++ ([5] ++ ([] ++ [9] ++ [])))
-- = ([1] ++ [1] ++ [2] ++
-- ++ [3] ++ ([4] ++ ([5] ++ [9]))
-- = [1,1,2,3,4,5,9]
--
-- Definir la función
-- ordenaRapida :: Ord a => [a] -> [a]
-- tal que (ordenaRapida xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
-- ordenaRapida [3,1,4,1,5,9,2] == [1,1,2,3,4,5,9]
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []  
ordenaRapida (x:xs) = ordenaRapida menores ++ [x] ++ ordenaRapida mayores
  where
    menores = [y | y <- xs, y <= x]  
    mayores = [y | y <- xs, y > x]   





------------------------------Nuevos tipos de datos------------
import Data.List(sortBy,maximumBy,minimumBy)
--1.- Crea un nuevo tipo Estudiate con los siguientes atributos
-- Nombre, Apellido, Edad, Número de control 
-- Genera una lista de un mínimo de 10 estudiantes en donde obtendras

-- Lista ordenada de los estudiantes de acuedo a la edad
-- Obtener al estudiante menor, mayor
-- Obtener el promedio de edades.
-- Definimos el tipo de dato 
data Estudiante = Estudiante {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    numControl :: Int
} deriving (Show, Eq)

estudiantes :: [Estudiante]
estudiantes = [
    Estudiante "Sofia" "Hernandez" 22 21160754,
    Estudiante "Gabriel" "Perez" 18 21160789,
    Estudiante "Sofia" "Hernandez" 22 21160689,
    Estudiante "Marcos" "Gonzalez" 23 21160678 ,
    Estudiante "Sofia" "Hernandez" 22 21160890,
    Estudiante "Emiliano" "Garcia" 19 21160980,
    Estudiante "Mario" "Lopez" 22 21160678,
    Estudiante "Ernesto" "Gonzalez" 21 21160980,
    Estudiante "Erick" "Gomez" 21 21160743,
    Estudiante "Ambar" "Ramirez" 20 21160894,
    Estudiante "Carina" "Ruiz" 21 2160893,
    Estudiante "Maria" "Gutierrez" 22 21160892
  ]

-- Función para ordenar la lista de estudiantes por edad
ordenaPorEdad :: [Estudiante] -> [Estudiante]
ordenaPorEdad = sortBy (\e1 e2 -> compare (edad e1) (edad e2))

-- Función para obtener al estudiante más joven
estudianteMasJoven :: [Estudiante] -> Estudiante
estudianteMasJoven = minimumBy (\e1 e2 -> compare (edad e1) (edad e2))


-- Función para obtener al estudiante más viejo
estudianteMayor :: [Estudiante] -> Estudiante
estudianteMayor = maximumBy (\e1 e2 -> compare (edad e1) (edad e2))

-- Función para calcular el promedio de edades
promedioEdad :: [Estudiante] -> Float
promedioEdad es = fromIntegral (sum (map edad es)) / fromIntegral (length es)



----------------------------Árboles--------------------
--Crea un nuevo tipo de dato árbol
--Funciones
--1.- Insertar desde un arreglo 
--2.- Buscar un elemento en un árbol
--3.- Recorridos (Inorden, posorden, preorden)


-- Definir el tipo de dato arbol
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

-- Función para insertar un elemento en un árbol binario de búsqueda
insert :: (Ord a) => a -> Arbol a -> Arbol a
insert x Hoja = Nodo x Hoja Hoja
insert x (Nodo valor izq der)
  | x < valor = Nodo valor (insert x izq) der
  | x > valor = Nodo valor izq (insert x der)
  | otherwise = Nodo valor izq der -- si el numero se inserta y ya hay uno, se mantiene o no se introduce

-- Función para insertar una lista de elementos en el árbol
insertarArreglo :: (Ord a) => [a] -> Arbol a
insertarArreglo = foldr insertar Hoja

-- Función para buscar un elemento en un árbol binario de búsqueda
buscar :: (Ord a) => a -> Arbol a -> Bool
buscar _ Hoja = False
buscar x (Nodo valor izq der)
  | x == valor = True
  | x < valor = buscar x izq
  | x > valor = buscar x der


