-- a constant

e :: Double
e = exp 1

-- Here we define a function

square :: Double -> Double
square x = x**2


-- function from LYAH
maximumo :: (Ord a) => [a] -> a
maximumo [] = error "maximum of empty"
maximumo [x] = x
maximumo (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximumo xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


