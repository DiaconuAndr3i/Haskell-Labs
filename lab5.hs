--Primele elemente din perechile unei liste
firstEl :: [(a,b)] -> [a]
firstEl [] = []
firstEl xs = map (\(x,_) -> x) xs

--Suma pentru fiecare sublista
sumList :: [[Int]] -> [Int]
sumList xs = map sum xs

--Cu recursivitate
prel2 :: [Int] -> [Int]
prel2 [] = []
prel2 (x:xs)
    | x `mod` 2 == 0    = x `div` 2: prel2 xs
    | otherwise         = x*2 : prel2 xs

prel2_faraRecursivitate :: [Int] -> [Int]
prel2_faraRecursivitate [] = []
prel2_faraRecursivitate xs = map (\ x -> if x`mod`2 == 0 then x`div` 2 else x*2) xs

ex4_lab5 :: Char -> [String] -> [String]
ex4_lab5 _ [] = []
ex4_lab5 c xs = filter (elem c) xs


ex5_lab5 :: [Int] -> [Int]
ex5_lab5 [] = []
ex5_lab5 xs = map (^2) (filter odd xs)

ex6_lab5 :: [Int] -> [Int]
ex6_lab5 [] = []
ex6_lab5 xs = map (^2) xs
