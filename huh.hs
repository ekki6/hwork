-- stuff added to test git
-- ch6x.hs compared to ch6.hs this one subsumes dt into 
-- trapIntegrate, and oddly enough it works

-- sqrt 1=x fn



squink :: Double -> Double
squink x = sqrt (1 +x)


-- rock throw

yRock30 ::  Double -> Double
yRock30 t = (-9.8) * t**2 + 30 * t

yRock ::  Double -> Double -> Double
yRock v0 t = (-9.8) * t**2 + v0 * t


vRock30 ::  Double -> Double
vRock30 t = (-9.8) * t +30


vRock ::  Double -> Double -> Double
vRock v0 t = (-9.8) * t + v0

greaterThanOrEq7' :: Int -> Bool
greaterThanOrEq7' n = n >= 10

blerg :: Int -> String -> Bool
blerg n s = (length s == n)

-- exercise 6.6 p88
--
sixMore :: [a] -> Bool
sixMore x = (length x > 6)

-- ex 6.8 bit - makes me nervous, use take 5 on this
sqrz = map (**2) [1..1000]

-- 6.9 repeat'

repeat' :: a -> [a]
repeat' n = iterate (\x -> x) n

-- 6.10 replicate from take and repeat
--
replicate' :: Int -> a -> [a]
replicate' n a = take n $ repeat a

-- 6.11 car velocity
--
vCar :: Double -> [Double]
vCar v0 = iterate (\x -> x + 5.0) v0

-- 6.12 map
--
map' :: (a -> b) -> [a] -> [b]
map' f x = [ f n | n <- x ]

-- 6.13 filter
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' f x = [ n | n <- x, f n ]

-- 6.14 average
-- using fromIntergral on sum messes this up
--
average :: [Double] -> Double
average x = sum x / (fromIntegral $ length x)

-- something else

-- 6.16 ugh
-- f! this stupid shit needs to work, but I'm
-- unable to put the dt inside the function,
-- maybe as I look at more examples


type R = Double

--dt :: R
--   -> R
--   -> R
--   -> R
--dt n a b = ((b - a) / n)

trapIntegrate   :: Integer      -- # of trapezoids n
                -> (R -> R)     -- function f
                -> R            -- lower limit a
                -> R            -- upper limit b
                -> R            -- result

trapIntegrate n f a b = 
    let
        --dt sigh
        dt :: R
        dt = (b - a) / (fromInteger n)
    in
        sum [(1/2)*(f t + f (t+dt))*dt | t <- [a, a+dt..(b - dt)]]

-------------------------------

sinDeg :: Double -> Double
sinDeg x = sin ( pi * x /180)

ff :: Double -> Double
ff x = x ** (1/3)

gg :: Double -> Double
gg x = exp x + 8 ** x

hh :: Double -> Double
hh x = ((x - 5) ** 2 +16) ** (-0.5)

gamma :: Double -> Double
gamma x = (1 - x ** 2) ** (-0.5)

uu :: Double -> Double
uu x = (10 + x ) ** (-1) + (10 - x) ** (-1)

ll :: Double -> Double
ll x = (x * (x + 1)) ** (-0.5)

ee :: Double -> Double
ee x = (abs x) ** (-3)

eeh :: Double -> Double
eeh x = (x ** 2 + 4) ** (-1.5)

