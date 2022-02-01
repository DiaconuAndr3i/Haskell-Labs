eeny :: Integer -> String
eeny = undefined
fizzbuzz :: Integer -> String
fizzbuzz = undefined
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
tribonacci :: Integer -> Integer
binomial :: Integer -> Integer -> Integer

verifL :: [Int] -> Bool

takefinal :: [a] -> Int -> [a]

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t
totalLen :: [String] -> Int




poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x =
    let
    in
        x*x*x+b*x+c

evenNum :: Integer -> String
evenNum x =
    if (even x == True)
        then
            "eeny"
        else
            "meeny"

fizbuz :: Integer -> String
fizbuz n
    | n`mod`3==0 &&  n`mod`5==0   ="FizzBuzz"
    | n`mod`3==0    ="Fizz"
    | n`mod`5==0    ="Buzz"
    | otherwise = ""

tribonacci n 
    | n < 3     = 1
    | n == 3     = 2
    | otherwise = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)

{-binomial n k
    | k == 0    = 1
    | n == 0    = 0
    |otherwise  = binomial (n-1) k + binomial (n-1) (k-1)-}

binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

verifL list
    | length list `mod` 2 == 0  = True
    | otherwise=    False

takefinal list n
    | length list < n   = list
    | otherwise     = reverse (take n (reverse list))

remove:: [a] -> Int -> [a]
remove list n =
    let
        aux = take (n+1) list
        aux1 = reverse aux
        aux2 = drop 1 aux1
        aux3 = reverse aux2
        list1 = drop (n+1) list
        list2 = aux3 ++ list1
    in
        list2



myReplicate :: Int -> Int -> [Int]
myReplicate 0 v = []
myReplicate n v =
    let
        ones = [v,v..]
        list = take n ones
    in
        list

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs) =
    if ( x`mod`2==1 )
        then
            x + sumImp(xs)
        else
            sumImp(xs)

totalLen [] = 0
totalLen (x:xs) = 
    if ( take 1 x == "A" )
        then
            length x + totalLen(xs)
        else
            totalLen(xs)
