-- exercism number 2

isLeapYear :: Integer -> Bool
isLeapYear year =
    if (mod year 4 == 0) && (not (mod year 400 ==0)) then True
    else if (mod year 100) == 0 then False
    else False

isLeapYear2 :: Integer -> Bool
isLeapYear2 year 
    | not (mod year 4 == 0) && (not (mod year 100 == 0) = False
    | (mod year 100 == 0) && (not (mod year 400 == 0)) = False
    | otherwise = True

