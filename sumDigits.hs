-- some fns for euler prob num 58

sumDigits :: Integer -> Integer
sumDigits = sum . map (read . (:[])) . show

 
ppowers :: Integer -> [Integer]
ppowers n = map (n^) [1..100]

--ghci> maximum (map  sumDigits (concat $ map ppowers [1..100]))
--972
