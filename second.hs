-- sqrt 1=x fn



squink :: Double -> Double
squink x = sqrt (1 +x)


-- rock throw

yRock30 ::  Double -> Double
yRock30 t = (-9.8) * t**2 + 30 * t



vRock30 ::  Double -> Double
vRock30 t = (-9.8) * t +30


-- something else


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

