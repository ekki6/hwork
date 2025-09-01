-- A function that takes an integer n and returns the square of (n+1)
outerFunction :: Int -> Int
outerFunction n =
    let
        -- inner function defined inside outerFunction
        innerFunction x = x + 1
    in
        (innerFunction n) ^ 2
