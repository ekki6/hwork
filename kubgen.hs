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

fourHash :: String -> String
fourHash = concatMap hexToFour

hash4 :: String
hash4 = fourHash hsh

plainText :: String
plainText = "here is some text"

plainLen :: Int
plainLen = length plainText

mixy119 :: [Char] -> Int
mixy119 x = mod (sum $ map fromEnum x) 119
-- or from chatGPT: mixy119 = (`mod` 119) . sum . map fromEnum

mixVal :: Int
mixVal = mixy119 plainText

hash4Val :: Int -> Int
hash4Val x = fromEnum (hash4 !! x) - fromEnum '0'

churn :: Int -> Int
churn x = mixVal + x
