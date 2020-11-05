-- task1
fact :: Int -> Int
fact n = if n <= 0 then 1 else n * fact (n-1)

-- task2
gcd' :: Int -> Int -> Int
gcd' a b = if (mod (max a b) (min a b)) == 0 
        then (min a b)
        else gcd' (min a b) (mod (max a b) (min a b))

-- task3
isPrime :: Int -> Bool
isPrime a = if a == 2 || a == 3
        then True
        else isPrime_ a 2

isPrime_ :: Int -> Int -> Bool
isPrime_ a n = if n^2 <= a
        then if mod a n == 0 then
                False
                else isPrime_ a (n + 1)
        else True

-- task4
reverseNumber :: Integer -> Integer
reverseNumber a = reverseNumber_ a (lengthNumber a)

reverseNumber_ :: Integer -> Integer -> Integer
reverseNumber_ a n = if n == 1 
        then a
        else (mod a 10) * 10 ^ (n - 1) + reverseNumber_ (div a 10) (n - 1)

lengthNumber :: Integer -> Integer
lengthNumber a = if a == 0
        then 0
        else 1 + lengthNumber (div a 10)

--  task5 
maxRoot :: Double -> Double -> Double -> Double
maxRoot a b c = if (b ^ 2 - 4 * a * c) < 0 
        then 0/0
        else (max ((-b - (sqrt (b ^ 2 - (4 * a * c))))/(2 * a)) ((-b + (sqrt (b ^ 2 - (4 * a * c))))/(2 * a)))

-- task6
root :: (Double->Double) -> Double -> Double -> Double -> Double
root f a b eps = if (f (a + b)/2) <= eps
        then (a + b)/2
        else  if (f (a + b)/2) * f a <= 0
                then root f a ((a + b)/2) eps
                else if (f (a + b)/2) * f b <= 0 
                        then root f ((a + b)/2) b eps
                        else 0/0