module Bob (responseFor) where

import Data.Char (isUpper, isLower)

responseFor :: String -> String
responseFor xs
    | any isLower xs && elem '?' xs = "Sure"
    | any isUpper xs && elem '?' xs = "Calm down, I know what I'm doing!"
    | any isLower xs = "Whatever"
    | any isUpper xs = "Whoa, chill out!"
    | otherwise = "Fine. Be that way!"
