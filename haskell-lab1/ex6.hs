absInt :: Int -> Int
absInt n | n >= 0    = n
         | otherwise = -n

sgn :: Int -> Int
sgn n   | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c)   | a <= b && a <= c = a
                    | b <= a && b <= c = b
                    | c <= b && c <= a = c

toUpper :: Char -> Char
toUpper c   | fromEnum c >= 97 && fromEnum c <= 122 = toEnum (fromEnum c - 32)
            | otherwise = c

toLower :: Char -> Char
toLower c   | fromEnum c >= 65 && fromEnum c <= 98 = toEnum (fromEnum c + 32)
            | otherwise = c

isDigit :: Char -> Bool
isDigit c   | fromEnum c >= 48 && fromEnum c <= 57 = True
            | otherwise = False

charToNum :: Char -> Int
charToNum c | isDigit c = fromEnum c - 48

romanDigit :: Char -> String
romanDigit c
  | c == '1' = "I"
  | c == '2' = "II"
  | c == '3' = "III"
  | c == '4' = "IV"
  | c == '5' = "V"
  | c == '6' = "VI"
  | c == '7' = "VII"
  | c == '8' = "VIII"
  | otherwise = "IX"