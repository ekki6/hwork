-- exercises for ch 3


floorx :: Double -> Double
floorx x = if x > 0
          then x
          else 0

erm :: Double -> Double
erm r = if r > 1
        then r ** (-2)
        else r

-- for below char must be in single quotes like isXorY 'Y'
isXorY :: Char -> Bool
isXorY k = case k of
             'X' -> True
             'Y' -> True
             _   -> False

bagfee :: Bool -> Int
bagfee checkingbags = if checkingbags
                      then 100
                      else 0

bagfee2 :: Bool -> Int
bagfee2 checkingbags = case checkingbags of
                         True -> 100
                         False -> 0

greaterThan50 :: Int -> Bool
greaterThan50 n = if n > 50
                  then True
                  else False

amazingCurve :: Int -> Int
amazingCurve n = if n > 50
                 then 100
                 else 2 * n

cR :: Double
cR = 3.5

