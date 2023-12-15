module HW1
import StdEnv

/*
	1. Given an integer 'n' and calculate the sum of odd number of fibonacci series from F1..Fn.
	For ex: 10 is given.
	Then, the first 10 fibonacci numbers are  1  1  2  3  5  8  13  21  34  55
	Sum of the odd numbers among them is: 1 + 1 + 3 + 5 + 13 + 21 + 55 = 99
	
	Note: Fibonacci series here start from 1. (F1 = 1, F2 = 1, F3 = 2)
*/

FibonacciOddSeries :: Int -> Int

// Start = FibonacciOddSeries 10		// 99
// Start = FibonacciOddSeries 16		// 1785
// Start = FibonacciOddSeries 0			// 0
// Start = FibonacciOddSeries 20		// 14328

/*
	2. Given a positive integer and a string value belongsto {"Even","Odd"} and write a function that returns the sum of the odd digits if the given string is "Odd" and
	returns the sum of even digits if the given string is "Even".
	
	String equality can be checked with == operator. "Even" == "Even" is true and "Odd" == "Odd" is true.
	
	For ex:
	123046 "Even" -> 2, 4 and 6 are the only Even digits among them, so the result is 12
	123046 "Odd" -> 1 and 3 are the only Odd digits among them, so the result is 4.
*/

// DigitSummation :: Int String -> Int

// Start = DigitSummation 123046 "Odd"		// 4
// Start = DigitSummation 123046 "Even"		// 12
// Start = DigitSummation 745209 "Even"		// 6
// Start = DigitSummation 745209 "Odd"		// 21
// Start = DigitSummation 353 "Odd"			// 11
// Start = DigitSummation 353 "Even"		// 0

