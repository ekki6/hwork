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
    all instDivBy lprimes

pushLprimes :: a -> [a]
pushLprimes 
