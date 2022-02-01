import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x

triple :: Integer -> Integer
triple x=x+x+x


--maxim :: Integer -> Integer -> Integer
maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y) then x else y

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = 
     if (x > y && x > z) 
          then x
          else 
               if ( y > x && y > z )
                    then y
                    else 
                         z


max3 x y z = let
             u = maxim x y
             in (maxim  u z)



maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 x y z k =
     let
          aux = maxim3 x y z
     in
          maxim aux k


sumSquare :: Integer -> Integer -> Integer
sumSquare x y =
     let 
          sum1 = x * x
          sum2 = y * y
     in 
          sum1 + sum2

evenOddFunction :: Integer -> String
evenOddFunction x =
     if ( x `mod` 2 == 0 )
          then 
               "Par"
          else 
               "Impar"

factNumber :: Integer -> Integer
factNumber n =
     if ( n == 0 )
          then 1
     else 
          n * factNumber(n-1)

functionVerif :: Integer -> Integer -> String
functionVerif a b =
     if (a > 2*b)
          then
               "Da"
          else 
               "Nu"

               


f 5 in f x = let x = 3 ; y = 4 in x + y