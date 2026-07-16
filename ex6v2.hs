module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = 
    all (`elem` lowered) ['a'..'z']
  where
    lowered = map toLower text
        
