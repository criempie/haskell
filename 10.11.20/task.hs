-- task1

dFact :: Integer -> Integer
dFact n 
    | n <= 1 = 1
    | otherwise = n * dFact (n - 2)

-- task2

sumOfDigits :: Integer -> Integer
sumOfDigits a 
    | a == 0 = 0
    | otherwise = (mod a 10) + sumOfDigits (div a 10) 

-- task3

powOf2 :: Integer -> Int
powOf2 a = powOf2_ a 1

powOf2_ :: Integer -> Int -> Int
powOf2_ a n 
    | a == 2 = n
    | odd a = -1
    | otherwise = powOf2_ (div a 2) (n + 1)

-- task4 
    -- а
task4_a_arg :: Integer -> Integer -> Integer
task4_a_arg x y = x ^ y

task4_a_w = (^)

    -- b
task4_b_arg :: Integer -> Integer -> Integer
task4_b_arg x y = (x + 1) ^ y

task4_b_w = (^) . (+) 1 

    -- c
task4_c_arg :: Integer -> Integer -> Integer 
task4_c_arg x y = x ^ (y - 3)

task4_c_w = flip $ flip (^) . flip (-) 3

    -- d
task4_d_arg :: Integer -> Integer -> Integer -- TO DO
task4_d_arg x y = (x + 1) ^ (y - 3)

-- task 5
