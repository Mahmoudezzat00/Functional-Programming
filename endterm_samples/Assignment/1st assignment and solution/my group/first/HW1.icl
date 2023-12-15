module HW1
import StdEnv

/* Please write your name and Neptun code here -        */

/* 1. Write GetLastPositive function which returns the last digit of the number if its positive 
and -1 if the number is negative */

GetLastPositive :: Int -> Int
GetLastPositive 0 = 0
GetLastPositive x
| x > 0 = x rem 10
| otherwise = -1

Start = GetLastPositive 5856 // 6  
//Start = GetLastPositive 689255 // 5
//Start = GetLastPositive 0 // 0
//Start = GetLastPositive 8 // 8
//Start = GetLastPositive -8554 // -1

/* 2. Given a year, determine if the year is a leap year or not
A year is a leap year if it is divisible by 4 but not divisible by 100.
But a year can be a leap year if it is divisible by 400 (eventhough it is divisible by 100).
If more explanation is needed check out this video - https://www.youtube.com/watch?v=xX96xng7sAE     */

//isLeapYear :: Int -> Bool

//Start = isLeapYear 1900 // False
//Start = isLeapYear 1997 // False
//Start = isLeapYear 1996 // True
//Start = isLeapYear 2000 // True

/* 3. Given a decimal number turn it into an octal number.
Octal number has eight as a base instead of 10 in decimal numbers.
In case you do not know the procedure please check out this source - https://electricalengineering123.com/octal-number-system-octal-to-decimal-binary-hexadecimal-conversion/    */

//toOctal :: Int -> Int

//Start = toOctal 1234 // 2322
//Start = toOctal 2023 // 3747
//Start = toOctal 467383847 // 3366733047
//Start = toOctal 0 // 0
//Start = toOctal -10 // "Negative number is not allowed"