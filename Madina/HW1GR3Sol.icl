module HW1GR3Sol
import StdEnv

//Please write your neptun code here:
/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.
*/






//Define a function to find the minimum number of currency notes to be returned by an ATM machine for a given amount of money.
//The currency notes available in the ATM are 10, 5,2 and 1.
//The input is always a positive integer.

//ATM :: Int -> Int

ATM :: Int -> Int
ATM x = ATMhelp x 0 // callin the helper function with the given amount and a counter set to 0


//Helper function for ATM
// x is the input and y is a counter for the number of notes.
// first we get rid of the 10s, then the 5s, then the 2s and finally the 1s.
// everytime we add a note to the counter, we subtract the value of the note from the input.
// Helper function is made because the given function ATM can only take one parameter.

ATMhelp :: Int Int -> Int
ATMhelp x y
| x >= 10 = ATMhelp (x - 10) (y + 1)      //If x is greater than or equal to 10, subtract 10 from input amount x and add 1 to counter y.
| x < 10 && x >= 5 = ATMhelp (x - 5) (y + 1) //If x is greater than or eual to 5 but less than 10, subtract 5 from input amount x and add 1 to counter y.
| x < 5 && x >= 2 = ATMhelp (x - 2) (y + 1) //If x is greater than or equal to 2 but less than 5, subtract 2 from input amount x and add 1 to counter y.
| x < 2 && x >= 1 = ATMhelp (x - 1) (y + 1) //If x is greater than or equal to 1 but less than 2, subtract 1 from input amount x and add 1 to counter y.
= y

//Start = ATM 100  // 10
//Start = ATM 99  // 12
//Start = ATM 28  // 5
//Start = ATM 1  // 1
//Start = ATM 3075927 



/*
      write a function to find the greatest common divisor of two numbers.
      If the gcd is one of the input numbers, then print "y is a multiple of x"
      otherwise print "neither number is a multiple of the other".
      If one of the input numbers is 0, then print "Cannot calculate gcd if  of 0" and abort.
      e.g: input: 6 18
           output: 6
           explanation: 6 is one of the input numbers, so print "either number is a multiple of the other"
      eg: input: 21 28
          output: 7
          explanation: 7 is a not any of the input numbers, so print "neither number is a multiple of the other"
*/

// Function to calculate GCD of two numbers
gcd :: Int Int -> Int
gcd x y
| y == 0 = x
= gcd y (x rem y)


//Function to check if the gcd is equal to any of the input numbers
IsGCDInputNum :: Int Int -> Bool
IsGCDInputNum x y
| gcd x y == x = True
| gcd x y == y = True
= False

// Main function to print the result
MyFunGCD :: Int Int -> String
MyFunGCD 0 x = abort "Cannot calculate gcd of 0"
MyFunGCD x 0 = abort "Cannot calculate gcd of 0"
MyFunGCD x y 
| IsGCDInputNum x y = "either number is a multiple of the other"
= "neither number is a multiple of the other"



//Start = MyFunGCD 6 18 // "either number is a multiple of the other"
//Start = MyFunGCD 21 28  // "neither number is a multiple of the other"
//Start = MyFunGCD 0 0 // "Cannot calculate gcd of 0" 




