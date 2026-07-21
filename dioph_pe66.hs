--for project euler #66 on Diophantine equations
--find minimal solution for Diophantine equation x^2 - Dy^2 = 1

diTest :: Integer -> Integer -> Integer
diTest d y = d * y * y + 1

testSqr :: Integer -> Integer
testSqr x = floor (sqrt (fromIntegral x :: Double)) 
