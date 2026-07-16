--find all primes below a certain value
--
lprimes :: [Int]
lprimes = [2,3,5,7,11]

tnums :: [Int]
tnums = [1..20]

isPrime :: Int -> Bool
isPrime x = 
    all (\p -> x `rem` p /= 0) lprimes
--    all  (`rem` x /= 0) lprimes
