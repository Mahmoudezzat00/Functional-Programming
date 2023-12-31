module moc_sol

import StdEnv 



/* 1. Parasitic Number

 A Parasitic number (in base 10) is a positive number which can be multiplied 
 by a certain n by moving the rightmost digit of its decimal representation 
 to the front.
 e.g. 102564 � 4 = 410256
 Given a positive integer number and n, write a function to determine whether 
 it is a Parasitic number or not.
*/

digits :: Int -> [Int]
digits 0 = []
digits x = [x rem 10] ++ digits (x / 10)

parasitic :: Int Int -> Bool
parasitic x n = reverse (digits x) == (tl p ++ [hd p]) 
where p = reverse (digits (x * n))

//Start =  reverse (digits (102564 * 4))
//Start = parasitic 102564 4 // True
//Start = parasitic 142857 5 // True
//Start = parasitic 714285 8 // False
//Start = parasitic 105263157894736842 2 // True



/* 2. Double Ones

 Given a list of integers, write a function which will keep only the numbers
 that contain at least two '1' digits. For example:
 [1,2,21,121,11,234131,111111,123,0,334] -> [121,11,234131,111111]
*/

check2 :: [Int] -> Bool
check2 x = sum (filter ((==)1) x) > 1

doubleOne :: [Int] -> [Int]
doubleOne list = [x \\ x <- list | check2 (digits x)]

//Start = doubleOne [1,2,21,121,11,234131,111111,123,0,334] // [121,11,234131,111111]
//Start = doubleOne [12,1,11,33] // [11]
//Start = doubleOne [11,111,21] // [11,111]
//Start = doubleOne [] // []
//Start = doubleOne [21,3,1] // []



/* 3. Multiples

 Given an n>0 integer value, write a function that creates the double, the triple
 and so on n-th multiple of the number.
*/

multiple :: Int -> [Int]
multiple x = [n*x \\ n <-[2..x]]

//Start = multiple 5 // [10,15,20,25]
//Start = multiple 2 // [4]
//Start = multiple 1 // []



/* 4. List difference
 
 Given two lists (A and B) containing sublists of integer numbers, 
 both A and B are of the same length,
 for every sublist in A and B, return the difference of the two sublists.  

 The difference is defined as follows:  
 The List L1-L2 consists of elements that are in L1 but not in L2. 
 For example if L1=[1,2,3] and L2=[3,5], then L1-L2=[1,2].
*/

solve4 :: [Int] [Int] -> [Int]
solve4 [] b = []
solve4 [a : as] b 
| isMember a b = solve4 as b
= [a] ++ solve4 as b

diff :: [Int] [Int] -> [Int]
diff x y = [a \\ a<-x | not (isMember a y)]

difference :: [[Int]] [[Int]] -> [[Int]] 
difference [] [] = []
difference [x : xs] [y : ys] = [diff x y : difference xs ys]   // solve4 x y

//Start = difference [[1..5]] [[4..7]] // [[1,2,3]]
//Start = difference [[1..10] , [10..15] , [1..4]] [[1..10] , [11..20] , [5]] // [[],[10],[1,2,3,4]]
//Start = difference [] [] // [] 


 
/*5. GoodNumbers
   Write a function that takes a list as an argument and counts how many numbers are:
   greater or equal to 10 AND less or equal to 99 AND divisible by 3.
*/

countGoodNums :: [Int] -> Int
countGoodNums list = length [x \\ x<-list | x >=10 && x<=99 && x rem 3==0]

//Start = countGoodNums [1,12,10,99] // 2
//Start = countGoodNums [12,15,30,33,39,96,99] // 7
//Start = countGoodNums [9, 10, 100, 102, 105] // 0
//Start = countGoodNums [] // 0


/* 6. Primes7

 Given a list of numbers, keep only the prime numbers that end with the digit 7
*/

check6 :: Int -> Bool
check6 x = length [y \\ y <- [1..x] | x rem y == 0 ] == 2 && x rem 10 == 7

primes7 :: [Int] -> [Int]
primes7 x = filter check6 x

//Start = primes7 [1..10] // [7]
//Start = primes7 [1..100] // [7,17,37,47,67,97]
//Start = primes7 [1..6] // []



/* 7. Property check

 Given a list of tuples, write a function to determine
 whether all of the tuples inside of the list hold the (Even, Odd) property.
 [(2,1),(2,3),(4,1)] = True
*/	

check7 :: (Int, Int) -> Bool
check7 (a, b) = isEven a && isOdd b 

solve7 :: [(Int, Int)] -> Bool
solve7 [] = True
solve7 [x : xs]
| check7 x = solve7 xs
= False

holdsTrue :: [(Int, Int)] -> Bool
holdsTrue [] = False
holdsTrue x = solve7 x

holdsTrue1 :: [(Int, Int)] -> Bool
holdsTrue1 [] = False
holdsTrue1 x = and [isEven a && isOdd b \\ (a,b)<-x]  

//Start = holdsTrue [(2,1),(2,3),(4,1)] // True
//Start = holdsTrue [(1,3),(2,3),(3,4)] // False
//Start = holdsTrue1 [] // False



/* 8. Super Digit

 We define super digit of an integer x using the following rules.
 If x has only 1 digit, then its super digit is x.
 Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.
 Here, the digit-sum of a number is defined as the sum of its digits.

 E.g  : super_digit(9875) = super_digit(9+8+7+5) 
                          = super_digit(29) 
                          = super_digit(2+9)
                          = super_digit(11)
                          = super_digit(1+1)
                          = super_digit(2)
                          = 2

 Given a list of integers, return a list containing the super digit
 of every number in the list.  
*/

solve8 :: Int -> Int
solve8 n 
| n < 10 = n
= solve8 (sum (digits n))

super_digit :: [Int] -> [Int]
super_digit [] = []
super_digit [x : xs] = [solve8 x] ++ super_digit xs

//Start = super_digit [148148148 , 9875 ] // [3,2]
//Start = super_digit [884555 , 456 , 2351 , 21587 , 88 ] // [8,6,2,5,7]
//Start = super_digit [] // [] 



/* 9. Powers 
 Given a list of integers and an integer, write a function which returns a list 
 which only contains the powers of the integer.
*/

check9 :: Int Int -> Bool
check9 1 b = True
check9 a b
| a rem b == 0 = check9 (a / b) b
= False 

powersList :: [Int] Int -> [Int]
powersList [] n = []
powersList [x : xs] n 
| check9 x n = [x : powersList xs n]
= powersList xs n

powersList1 x n = [a \\ a<-x | check9 a n]

//Start = powersList1 [2,4,8,16,32,33,55] 2 // [2,4,8,16,32]
//Start = powersList [] 3 // []
//Start = powersList [1..10] 3 // [1,3,9]
//Start = powersList [-1,-2,4,8] 4 // [4]



/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/

isPrime :: Int -> Bool 
isPrime x = length [y \\ y <- [1..x] | x rem y == 0 ] == 2

isTwin :: Int Int -> Bool
isTwin l r
| (l + 2) > r = False
= isPrime l && isPrime (l+2)

twinPrimes :: Int Int -> Int
twinPrimes l r 
| l > r = 0
| isTwin l r = 1 + twinPrimes (l + 1) r
= twinPrimes (l + 1) r

//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0
//Start = twinPrimes 0 -5 // 0