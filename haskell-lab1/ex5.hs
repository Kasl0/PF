sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n < 0
    then -n
    else n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a <= b
    then a
    else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if a <= b && a <= c
    then a
    else if b <= a && b <= c
        then b
        else c

min3Int2 :: (Int, Int, Int) -> Int
min3Int2 (a, b, c) = min2Int (min2Int (a, b), min2Int (a, c))

toUpper :: Char -> Char
toUpper c = if fromEnum c >= 97 && fromEnum c <= 122
    then toEnum (fromEnum c - 32)
    else c

toLower :: Char -> Char
toLower c = if fromEnum c >= 65 && fromEnum c <= 98
    then toEnum (fromEnum c + 32)
    else c

isDigit :: Char -> Bool
isDigit c = fromEnum c >= 48 && fromEnum c <= 57

charToNum :: Char -> Int
charToNum c = fromEnum c - 48

romanDigit :: Char -> String
romanDigit c = if c == '1' then "I"
    else if c == '2' then "II"
    else if c == '3' then "III"
    else if c == '4' then "IV"
    else if c == '5' then "V"
    else if c == '6' then "VI"
    else if c == '7' then "VII"
    else if c == '8' then "VIII"
    else "IX"
