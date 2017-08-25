import Data.List
import System.IO
--

-- List => [ How much elements are less than temp ] 		[1,2,3] ===> [0,1,2]
less :: [Integer] -> [Integer]
less list = [ countLess x list 0 | x <- list ]

countLess t [] c = c
countLess t (x:xs) c = if t > x then countLess t xs (c+1)
                       else countLess t xs c

-- Sum of even elements 									[2,3,4,4] => 10
evenSum :: [Integer] -> Integer
evenSum list = accumSum 0 list
    where 
        accumSum n [] = n
        accumSum n (x:xs) = if even x
							then accumSum (n+x) xs
							else accumSum n xs

-- OR
evenSum2 list = sum (filter even list)

-- Sum of odd elements
oddSum list = sum (filter odd list)

anotherOddSum list = ( sum list ) - sum (filter even list)

-- Double each element
doubleValues list = [ x*2 | x <- list ]

-- Leaves only even elements
evensOnly :: [Int] -> [Int]
evensOnly xs = filter even xs

-- Number of digits needed for numbering N pages book
digNumber n = sum [ digitsIn(x) | x <- [1..n]]

digitsIn k = countDigits k 0

countDigits :: Integer -> Integer -> Integer
countDigits k c  | k == 0     = c
                 | otherwise = countDigits (k `div` 10) (c+1)

-- Sum of all even elements øncreased by three
sumList1 :: [Integer] -> Integer
sumList1 lst = if null lst then 0 
               else let x = head lst
                        xs = tail lst in (if even x then 3+x else 0) + (sumList1 xs)
						
-- OR
sumList2 []                = 0
sumList2 (x:xs) | even x   = (3 + x) + sumList2 xs
                | otherwise = sumList2 xs

-- OR
sumList3 = sum.(map(3+)).(filter even)

-- Reversing
myReverse []      =  []
myReverse (x:xs)  =  (myReverse xs) ++ [x]

-- Area of a triangle
square a b c = let p = (a + b + c) / 2
               in  sqrt (p * (p - a) * (p - b) * (p - c))

-- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- OR
factorial2 n = product [1..n]

-- Nth Fibonacci number
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- OR
fib2 x
    |x < 2 = 1
    |otherwise = fib2 (x - 1) + fib2 (x - 2)

-- First N Fibonacci numbers list
fibList :: Integer -> [Integer]
fibList x = [ fib n | n <- [1..x] ]

-- Infinity list of prime numbers
primes :: [Integer]
primes = [ x | x <- [2..], (check x [2..x-1])] 

check :: Integer -> [Integer] -> Bool
check _ [] = True
check a (x:xs)
 | (a `mod` x > 0) = check a xs
 | otherwise =  False