--find all primes below a certain value
--
lprimes :: [Int]
lprimes = [2,3,5,7,11]

tnums :: [Int]
tnums = [1..20]

isntDivBy :: Int -> Int -> Bool
isntDivBy x y = rem x y /= 0

isPrime :: Int -> Bool
isPrime x = 
--    all (\p -> rem x p /= 0) lprimes
    all (isntDivBy x) lprimes

isPrime2 :: Int -> Bool
isPrime2 x = 
--    all (\p -> rem x p /= 0) lprimes
    all (isntDivBy x) [2..(floor (sqrt (fromIntegral x)))]

--pushLprimes :: a -> [a]
--pushLprimes 

primesBelow :: Int -> [Int]
primesBelow n = [x | x <- [2..n-1], isPrime x]

omniPrimes :: [Int]
omniPrimes = filter isPrime2 [2..]

primesUnder ::  Int -> [Int]
primesUnder n = takeWhile (<n) omniPrimes
