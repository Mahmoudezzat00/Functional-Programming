module midterm2

import StdEnv 


// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> 
//for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither made it public.
//Students� regulation of Eotvos Lorand University 
//(ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student�s work - 
//or at least the significant part of it - as his/her own performance, 
//it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be 
//dismissal of the student from the University.


/* 1. Armstrong number

 If sum of cubes of each digit of the number is equal to the number itself,
 then the number is called an Armstrong number.
 153 = 1^3 + 5^3 + 3^3
 Given a positive integer number, write a function to determine whether it is
 an Armstrong number or not.
*/

splitDigit :: Int -> [Int]
splitDigit x 
| x < 10 = [x]
= splitDigit (x/10) ++ [x rem 10] 

armstrong :: Int -> Bool
armstrong n = sum (map (\x = x^3) list) == n 
where list = splitDigit n

//Start = armstrong 153 // True
//Start = armstrong 370 // True
//Start = armstrong 0 // True
//Start = armstrong 12 // False


/* 2. Occurrences

 Given a list of integers, replace every element in the list with its number
 of occurrences in the list.
*/
ocN :: [Int] [Int] -> [Int]
ocN [] list = []
ocN [x:xs] list = [(length occ)] ++ ocN xs list
where occ = filter ((==) x) list

occNum :: [Int] -> [Int]
occNum list = ocN list list

//Start = occNum [1,1,1,1,2,3,2,5,6,2,2,2,5] // [4,4,4,4,5,1,5,2,1,5,5,5,2]
// Start = occNum [1..5] // [1,1,1,1,1]
// Start = occNum ([1..5] ++ [1..7]) // [2,2,2,2,2,2,2,2,2,2,1,1]
// Start = occNum([7..9] ++ [7..9] ++ [7..9]) // [3,3,3,3,3,3,3,3,3]


/* 3. Gap2
 
 Given a list of numbers, convert the list in such a way that 
 the difference between two consecutive elements is always 2,
 you may have to add numbers in order to achieve that.
 e.g: [1,5,8] = [1,3,5,7,9]
*/

gap2 :: [Int] -> [Int]
gap2 [] = []
gap2 list = takeWhile (\x = x <= (last list) + 1) [1,3..]

//Start = gap2 [1,5,8] // [1,3,5,7,9]
//Start = gap2 [1,5,15] // [1,3,5,7,9,11,13,15]
//Start = gap2 [] 


/* 4. Not Palindrome
 Given a list of lists of integers,
 write a function that gets rid of Palindrome numbers.
 A palindrome number is a number that can be read from left to right or
 from right to left and gets the same number, 
 e.g. 12521 is a palindrome number. 
*/
pal :: Int -> Bool
pal n = list == (reverse list) && (length list > 1)
where list = splitDigit n

getrid :: [Int] -> [Int]
getrid list = filter (not o pal) list

getRidPal :: [[Int]] -> [[Int]]
getRidPal list = map getrid list

//Start = getRidPal [[1,2,1111],[151,22,3455]] // [[1,2],[3455]]
//Start = getRidPal [[1,222],[151,202,50505]] // [[1],[]]
//Start = getRidPal [[],[22]] // [[],[]]


/* 5. Not Primes
 Given a list of integers, write a function which removes the prime numbers   from the list.
 There will be no negative integers and consider the number 1 not a prime.
*/

isPrime :: Int -> Bool
isPrime n 
| n < 2 = False
= length([x \\ x <- [1..n] | n rem x == 0]) == 2

removeNotPrime :: [Int] -> [Int]
removeNotPrime list = filter (not o isPrime) list

//Start = removeNotPrime [1..10] // [1,4,6,8,9,10]
//Start = removeNotPrime [13..20] // [14,15,16,18,20]
//Start = removeNotPrime [2,4,8,9,10,23] // [4,8,9,10]
//Start = removeNotPrime [] // []


/* 6. zipWith

 Implement the function zipWith that takes a function, 
 and two lists, and combines them in such a way that 
 elements that are in the same positions get the function 
 applied to them.

 E.g: zipWith addTwoNumbers [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/
//DON'T DELETE THESE FUNCTIONS !!!
addTwoNumber x y = x + y
prodTwoNumber x y = x * y
niceTwoNumber x y = x rem y
//

// Start=zip2 [1 ,2 ,3] [’a’..’c’]             // [(1,’a’),(2,’b’),(3,’c’)]
// Start=zip ( [1 ,2 ,3] , [’a’..’c’] )          // [(1,’a’),(2,’b’),(3,’c’)]

zipWith :: (Int Int -> Int) [Int] [Int] -> [Int]
zipWith f [] [] = []
zipWith f [x:xs] [y:ys] = [f x y] ++ (zipWith f xs ys)

//Start = zipWith addTwoNumber [1,2,3] [5,6,7] // [6,8,10]
//Start = zipWith prodTwoNumber [1,2,3] [5,6,7] // [5,12,21]
//Start = zipWith niceTwoNumber [5,6,7] [1,2,3] // [0,0,1]


/* 7. Collatz conjecture

 Given a positive number greater than 1, return how many iterations does it 
 take for that number to fall down to "2" if we keep applying the
 Collatz equation on it.
 Collatz conjecture equation:
 If the number is even -> x/2
 If the number is odd -> 3x+1
 e.g: input: 10 
      steps: 10 -> 5 -> 16 -> 4 -> 2
      output: 4 recursive calls
*/
coll :: Int Int -> Int
coll n steps
| n == 2 = (steps-1)
| isEven n = coll (n/2) (steps+1)
= coll (3*n + 1) (steps+1)

collatz :: Int -> Int
collatz 0 = 0
collatz n = coll n 0

//Start = collatz 10 // 4
//Start = collatz 20000000 // 144
//Start = collatz 5 // 3
// Start = collatz 0 // "The number must be greater than 1"


/* 8. Good Lists

 Given a list of lists, count how many of the sublists are good lists.
 A list is good if it is empty or its 1st number is odd, 2nd is even, 
 3rd is odd, 4th is even and so on.
 E.g: [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] your function should return
 3 as only [], [1,2,3,4] and [9] are "good".
*/
goodlist :: [Int] -> Bool 
goodlist [] = True
goodlist [x,y:xs] 
|(isOdd x) && (isEven y) && goodlist xs = True
|otherwise = False

isGood :: [[Int]] -> Int
isGood listoflists = length (filter goodlist listoflists)

Start= isGood [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] // 3
// Start = isGood [[1,2,3,4],[3,4,4],[3,42],[12,2,1,2]] // 2
// Start = isGood [[],[1,2,3,4],[],[8,3,4],[1],[2],[9],[3,4,4]] // 5
// Start = isGood [] // 0


/* 9. Symmetrical lists
 Given a list of lists of integers, write a function 
 which returns a list of symmetrical lists, 
 if the sum of the sublist is greater than 10.
*/

symSumGreater10 :: [[Int]] -> [[Int]]
symSumGreater10 [] = []
symSumGreater10 [x:xs] 
| sum x > 10 = [x ++ (reverse x)] ++ symSumGreater10 xs
= symSumGreater10 xs

//Start = symSumGreater10 [[1,2,3],[3,4,5,6],[4,5,1,2]] // [[3,4,5,6,6,5,4,3],[4,5,1,2,2,1,5,4]]
//Start = symSumGreater10 [] // []
//Start = symSumGreater10 [[1..10],[1,2]] // [[1,2,3,4,5,6,7,8,9,10,10,9,8,7,6,5,4,3,2,1]]

 
/* 10. Elements in interval

 Given a list of triple tuples consisting of two integer values and 
 and a list of integers (left,right,[Int]),
 for every tuple return only the elements from the list 
 which positions' are inside the interval [left..right]
 Assume that the indexes are all valid.
*/

eInInterval :: (Int ,Int,[Int])-> [Int]
eInInterval (r,l,inter) = (drop r (take (l+1) inter))

elementInInterval :: [(Int ,Int,[Int])]-> [[Int]]
elementInInterval list = map eInInterval list

//Start = elementInInterval [(2,5,[1..10])] //[[3,4,5,6]]
//Start = elementInInterval [(5,6,[1..8]), (3,5,[4..9])] //[[6,7],[7,8,9]]
//Start = elementInInterval [(4,7,[1,2,3,4,5,6,7,8,9])] //[[5,6,7,8]]

