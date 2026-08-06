-- kubgen in haskell
-- updated for RFNG-2 hash (Robbie's Fragile
-- Number Garden version 2) :)
-- change plainText to the text you want to hash
-- to dispaly the kubgen numbers:
-- main = mapM_ putStrLn (krows churnHash)
-- to dispaly the kubgen  picture:
-- main = mapM_ putStrLn (krows kpicture)


-- hsh is the sort of initialization vector for the hash
-- it's a 128 digit hex.  I forget where it came from
hsh :: String
hsh= concat
    [ "7ef0742612e4db668ea2a7b5779f69cf"
    , "13762b48d36812fd72eda69207460a71"
    , "d2e2c901b8f46991249bd566e98b9e2b"
    , "31e897029c54c20f57a31e8e23f452f"
    ]

-- this converts a hex digit to base 4 for the
-- 'fourhash' I know it's not elegant but it's
-- clear :)
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

-- converts back to hex digit
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

-- makes the fourhash
fourHash :: String -> String
fourHash = concatMap hexToFour

-- constant, the init vector's hash
hash4 :: String
hash4 = fourHash hsh

plainText :: String
plainText = "here is some different text"

plainLen :: Int
plainLen = length plainText

-- mixy converts all charaters to their code numbers,
-- sums them and then mods it with 119
mixy119 :: [Char] -> Int
mixy119 x = mod (sum $ map fromEnum x) 119
-- or from chatGPT: mixy119 = (`mod` 119) . sum . map fromEnum

-- the static mixy value we will use for this string
mixVal :: Int
mixVal = mixy119 plainText

-- takes a digit from a fourhash and returns a number 0..3
hash4Val :: Int -> Int
hash4Val x = fromEnum (hash4 !! x) - fromEnum '0'

-- takes a character from the plaintext and returns 
-- its char value
plainVal :: Int -> Int
plainVal x = fromEnum (plainText !! x)

-- main hash functions, steps through the 256 values of
-- of the initial fourhash vector (really, because of 
-- mod 127 does less than half) and takes the xth value
-- of the plain text, adds the init 4hash with index
-- offset by mixy, adds mixy, and adds 1 and does mod 4
-- producing a 0 through 3
churn :: Int -> Int
churn x = mod (mixVal + (hash4Val (mod (x +mixVal) 127)) +
    plainVal (mod x plainLen) + 1) 4

-- puts the 256 [0..3]s into a string to be displayed
churnHash :: String
churnHash = concatMap (show . churn) [0..255]

-- breaks 4hash into pairs for conversion to hex
pairs :: String -> [String]
pairs [] = []
pairs (x:y:xs) = [x, y] : pairs xs
pairs _ = error "pairs: odd-length string"

-- applies pairs (above) to churnHash
churnHex :: String
churnHex = map fourToHex (pairs churnHash)

-- breaks 256 char string into 16 16 char strings
-- to make grid
krows :: String -> [String]
krows [] = []
krows xs = take 16 xs : krows (drop 16 xs)

--main = mapM_ putStrLn (krows churnHash)

-- alternate glyph representation of the kubgen
glyph :: Char -> Char
glyph '0' = ' '
glyph '1' = '.'
glyph '2' = 'o'
glyph '3' = '#'
glyph  _  = '?'

kpicture :: String
kpicture = map glyph churnHash

--main = mapM_ putStrLn (krows kpictire)
