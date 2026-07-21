--for project euler #66 on Diophantine equations
--find minimal solution for Diophantine equation x^2 - Dy^2 = 1

diTest :: Integer -> Integer -> Integer
diTest d y = d * y * y + 1

-- takes the sqrt() of a n and floors it
-- producing a new number which, if n is a perfect square,
-- when squared should give us back n
testSqr :: Integer -> Integer
testSqr x = floor (sqrt (fromIntegral x :: Double)) 

isSqr :: Integer -> Bool
isSqr x = (testSqr x)^2 == x

