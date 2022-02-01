
isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> Int
countVowels s = length $ filter isVowel s

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs) = 
    if ( x == reverse x )
        then
            countVowels(x) + nrVocale(xs)
        else
            nrVocale(xs)


f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs) =
    if ( x`mod`2==0 )
        then
            (x:)[n] ++ f n xs
        else
            (x:) (f n xs)

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]


isFactorOf :: Int -> Int -> Bool
isFactorOf a b = b `mod` a == 0

divizori :: Int -> [Int]
divizori n = [x | x <- [1..n], n`mod`x==0]

listadiv :: [Int] -> [[Int]]
listadiv list = [divizori x | x <- list]

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b list = [x | x <- list, x >= a, x<=b]

pozitive :: [Int] -> Int
pozitive l = length [x | x <- l, x > 0]

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare [] = []
pozitiiImpare list = [snd pair | pair <- list `zip` [0,1..], fst pair `mod` 2 == 1]

--Inserare pe o pozitie i
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as


isDigit :: Char -> Bool
isDigit c = elem c "0123456789"

justDigits :: String -> String
justDigits s = filter isDigit s

multiDigits :: String -> [Char]
multiDigits str = [x  | x <- str ]


