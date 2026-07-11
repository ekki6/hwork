module Bob (responseFor) where

import Data.Char (isUpper, isLower)

responseFor :: String -> String
responseFor xs
    | hasLower xs && hasQ xs= "Sure"
    | hasUpper xs && hasQ xs= "Calm down, I know what I'm doing!"
    | hasLower xs = "Whatever"
    | hasUpper xs = "Whoa, chill out!"
    | otherwise = "Fine. Be that way!"
    where
        hasUpper xs = any isUpper xs
        hasLower xs = any isLower xs
        hasQ xs = elem '?' xs
