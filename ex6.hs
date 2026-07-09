module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = 
    elem 'a' (map toLower text) &&
    elem 'b' (map toLower text) &&
    elem 'c' (map toLower text) &&
    elem 'd' (map toLower text) &&
    elem 'e' (map toLower text) &&
    elem 'f' (map toLower text) &&
    elem 'g' (map toLower text) &&
    elem 'h' (map toLower text) &&
    elem 'i' (map toLower text) &&
    elem 'j' (map toLower text) &&
    elem 'k' (map toLower text) &&
    elem 'l' (map toLower text) &&
    elem 'm' (map toLower text) &&
    elem 'n' (map toLower text) &&
    elem 'o' (map toLower text) &&
    elem 'p' (map toLower text) &&
    elem 'q' (map toLower text) &&
    elem 'r' (map toLower text) &&
    elem 's' (map toLower text) &&
    elem 't' (map toLower text) &&
    elem 'u' (map toLower text) &&
    elem 'v' (map toLower text) &&
    elem 'w' (map toLower text) &&
    elem 'x' (map toLower text) &&
    elem 'y' (map toLower text) &&
    elem 'z' (map toLower text)
    


--make list a to z
--['a'..'z']
--make test list lower case
--import Data.Char (toLower)
--map toLower "Hi There"
--test each letter of list 
-- >>> all (> 3) [1,2,3,4,5]
--False
--until letter not found - then False
--if get all the way through the list then True
--
