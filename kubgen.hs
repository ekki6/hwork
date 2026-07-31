-- kubgen in haskell

hsh :: String
hsh= concat
    [ "7ef0742612e4db668ea2a7b5779f69cf"
    , "13762b48d36812fd72eda69207460a71"
    , "d2e2c901b8f46991249bd566e98b9e2b"
    , "31e897029c54c20f57a31e8e23f452f"
    ]

hexToFour :: Char -> [Char]
hexToFour '0' = "00"
hexToFour '1' = "01"
hexToFour '2' = "02"
hexToFour '3' = "03"
hexToFour '4' = "10"
hexToFour '5' = "11"
hexToFour '6' = "12"
hexToFour '7' = "13"
hexToFour '8' = "20"
hexToFour '9' = "21"
hexToFour 'a' = "22"
hexToFour 'b' = "23"
hexToFour 'c' = "30"
hexToFour 'd' = "31"
hexToFour 'e' = "32"
hexToFour 'f' = "33"
hexToFour c = error ("Not a hex digit" ++ [c])

fourToHex :: String -> Char
fourToHex "00" = '0'
fourToHex "01" = '1'
fourToHex "02" = '2'
fourToHex "03" = '3'
fourToHex "10" = '4'
fourToHex "11" = '5'
fourToHex "12" = '6'
fourToHex "13" = '7'
fourToHex "20" = '8'
fourToHex "21" = '9'
fourToHex "22" = 'a'
fourToHex "23" = 'b'
fourToHex "30" = 'c'
fourToHex "31" = 'd'
fourToHex "32" = 'e'
fourToHex "33" = 'f'
fourToHex c = error ("Not expected kubgen fourSet" ++ c)

fourHash :: String -> String
fourHash = concatMap hexToFour

hash4 :: String
hash4 = fourHash hsh

plainText :: String
plainText = "here is some different text"

plainLen :: Int
plainLen = length plainText

mixy119 :: [Char] -> Int
mixy119 x = mod (sum $ map fromEnum x) 119
-- or from chatGPT: mixy119 = (`mod` 119) . sum . map fromEnum

mixVal :: Int
mixVal = mixy119 plainText

hash4Val :: Int -> Int
hash4Val x = fromEnum (hash4 !! x) - fromEnum '0'

plainVal :: Int -> Int
plainVal x = fromEnum (plainText !! x)

churn :: Int -> Int
churn x = mod (mixVal + (hash4Val (mod (x +mixVal) 127)) +
    plainVal (mod x plainLen) + 1) 4

churnHash :: String
churnHash = concatMap (show . churn) [0..255]

pairs :: String -> [String]
pairs [] = []
pairs (x:y:xs) = [x, y] : pairs xs
pairs _ = error "pairs: odd-length string"

churnHex :: String
churnHex = map fourToHex (pairs churnHash)
