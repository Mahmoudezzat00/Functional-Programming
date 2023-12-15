module cons1 
import StdEnv 
// 21. Given two integers, put their digits together like: 123 456 =123456
glue :: Int Int -> Int
glue a b = toInt (toString a +++ toString b) 
// Given a number, split the digits into a list
// 123 => [1,2,3]
// splitIntoList ::  Int -> [Int]
// splitIntoList num 
// | num < 10  = [num] 
// = splitIntoList (num / 10) ++ [num rem 10] 
// Start = splitIntoList 123  

countDigits :: Int -> Int 
// countDigits a = size (toString a)
countDigits n  
| n < 10  = 1
= 1 +  countDigits (n / 10) 
// Start = countDigits 456 // 3
// countDigits 456
// = 1 + countDigits (45)  =>  1 + 1 +  countDigits (4) =>   1 + 1 + 1   = 3
// "123" => {'1','2','3'}
// Given a three digit number, check 
//     if the middle digit of this number is odd or even;
//     E.g:
//         154  : the middile digit is 5 and it is odd => output should be "Odd"
//         545 :  the middile digit is 4 and it is even => output should be "Even"
// check :: Int -> String  
// check n = (toString n).[1] 
// Start = check 144
// toInt (toString ('1')) 
// Given a positive integer, find the sum of the odd numbers up to that number starting from 1.
sumOdd :: Int -> Int 
sumOdd n 
| n <= 1 = 1
| isOdd n = n + sumOdd (n - 1)
= sumOdd (n - 1) 
1) sumOdd 5
2)  5 + sumOdd (4)
3)  5 +  sumOdd (3)
4)  5 + 3 + sumOdd (2)
5)  5 + 3 + sumOdd (1)
7)  5 + 3 + 1 = 9 
// Start = sumOdd 5 // 9 
// Start = sumOdd 21 // 121
// Start = sumOdd 10 // 25 = 9+7+5+3+1
// Start = sumOdd -13 // n has to be positive