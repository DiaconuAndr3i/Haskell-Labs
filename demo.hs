--main = print([head (reverse [0,0,0,0,0,0,0])..])

import Data.Char
f :: Int -> Int
f x = x + x
g :: Int -> Int
g x = x * x

f1 :: [a] -> Int -> [a]
f1 [] _ = []
f1 list a = take a list

f2 :: [a] -> [a]
f2 [] = []
f2 list = reverse list

-- Lista de patrate
squares :: [Int] -> [Int]
squares [] = []
squares ( x : xs ) = x*x : squares xs

ords :: [Char] -> [Int]
ords [] = []
ords list = [ord x | x<- list]

ordsWithMap :: [Char] -> [Int]
ordsWithMap list = map ord list

-- Cifre din sir
digits :: [Char] -> [Char]
digits []       = []
digits xs    = [x | x <- xs, isDigit x]

str = ["cezare","petru","claudia","","virgil"]
maxLengthFn = foldr max 0 . 
               map length . 
               filter testC 
    where   testC('c':_) = True 
            testC _ = False

maxLength = maxLengthFn str


prefixComun :: String -> String -> String
prefixComun "" _ = ""
prefixComun _ "" = ""
prefixComun (x:xs) (y:ys)
  | x == y = x : (prefixComun xs ys)
  | otherwise = ""



rev = foldl (<:>) []
  where (<:>) = flip (:)

main = print(prefixComun "sirulnr1" "sirdoi")



