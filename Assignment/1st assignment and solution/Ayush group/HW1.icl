module HW1
import StdEnv

/*
	1. A positive integer named 'n' is given and if it is even, then divide by 2 (n/2), if it is odd, then 3n+1.
	Repeat this process by recursion until the value reaches to 1.
	
	Write a function that calculates how many steps went through ODD Number in this process for an integer number given.
	
	For ex: 
	the given integer is 6: 
		as it is even, divide by 2 -> 
		
		1. 6/2 = 3 		-> 1
		2. (3*3)+1 = 10 -> 0
		3. 10/2 = 5 	-> 1
		4. (5*3)+1 = 16 -> 0
		5. 16/2 = 8 	-> 0
		6. 8/2 = 4 		-> 0
		7. 4/2 = 2 		-> 0
		8. 2/2 = 1 		-> 0
		total 8 steps, but only 2 steps went through the odd number, so the function should return 2.	
*/

CollatzConjunctureOdd :: Int -> Int

CollatzConjunctureOdd num
| num == 1 = 0
| isEven num =  CollatzConjunctureOdd (num/2)
= 1 + CollatzConjunctureOdd ((3*num) + 1)


// Start = CollatzConjunctureOdd 6		// 2
// Start = CollatzConjunctureOdd 27		// 41
// Start = CollatzConjunctureOdd 9		// 6
// Start = CollatzConjunctureOdd 97		// 43

/*
	2. Given a positive integer and a string value belongsto {"Even","Odd"} and write a function that returns the sum of the odd digits if the given string is "Odd" and
	returns the sum of even digits if the given string is "Even".
	
	String equality can be checked with == operator. "Even" == "Even" is true and "Odd" == "Odd" is true.
	
	For ex:
	123046 "Even" -> 2, 4 and 6 are the only Even digits among them, so the result is 12
	123046 "Odd" -> 1 and 3 are the only Odd digits among them, so the result is 4.
*/


DigitSummation :: Int String -> Int

DigitSummation num str
| num == 0 = 0
| isOdd (num rem 10) && (str == "Odd") = (num rem 10) + DigitSummation (num/10) str
| isEven (num rem 10) && (str == "Even") = (num rem 10) + DigitSummation (num/10) str
= DigitSummation (num/10) str

// Start = DigitSummation 123046 "Odd"		// 4
// Start = DigitSummation 123046 "Even"		// 12
// Start = DigitSummation 745209 "Even"		// 6
// Start = DigitSummation 745209 "Odd"		// 21
// Start = DigitSummation 353 "Odd"			// 11
// Start = DigitSummation 353 "Even"		// 0

