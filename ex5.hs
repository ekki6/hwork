module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury t = t / (0.2408467 * 31557600)
ageOn Venus t = t / (0.61519726 * 31557600)
ageOn Earth t = t / (1 * 31557600)
ageOn Mars t = t / (1.8808158 * 31557600)
ageOn Jupiter t = t / (11.862615 * 31557600)
ageOn Saturn t = t / (29.447498 * 31557600)
ageOn Uranus t = t / (84.016846 * 31557600)
ageOn Neptune t = t / (164.79132 * 31557600)
--ageOn x t = 0.0 not needed as all values matced
--ageOn _ _ = 0.0 is another option

earthSecs :: Float -> Float
earthSecs yr = yr * 31557600

--ageOn planet seconds = error "You need to implement this function."

--tplan :: Planet -> Double
--tplan planet
--    | planet == Mercury = 0.2408467
--    | otherwise = 1
