factori :: Int -> [Int]
factori 0 = [1,2..]
factori n = [x | x <- [1..n], n`mod`x==0]

prim :: Int -> Bool
prim 1 = False
prim n
    | length (factori n) == 2     = True
    | otherwise                 = False

numerePrime :: Int -> [Int]
numerePrime n = [ x | x <- [2..n], prim x ]

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 _ _ [] = []
myzip3 _ [] _ = []
myzip3 [] _ _ = []
myzip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myzip3 xs ys zs

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [ fst pair < snd pair | pair <- (x:xs) `zip` xs ]

ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata list f = and [\f (fst pair) (snd pair) | pair <- (x:xs) `zip` xs, \f x y -> x$y ]