module HW3
import StdEnv

//Please write your name and neptun code: FOZLQN

/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code. */

/* You are given a list. The task is to replace each element of the list with its next greater element.
If there is no such number, replace it with 0.
For example: 
[2,1,5] -> [5,5,0] As 1 is less than 2, the next number greater than 2 is 5 and so on. */
/*



nextGreater :: [Int] -> [Int]
nextGreater [] = []
nextGreater [x:xs] = [findNextGreater x xs] ++ nextGreater xs

findNextGreater :: Int [ Int] -> Int
findNextGreater _ [] = 0
findNextGreater x [y:ys] | y > x     = y
                         | otherwise = findNextGreater x ys

//Start = nextGreater [2,1,5] // [5,5,0]
//Start = nextGreater [9] // [0]
//Start = nextGreater [2,7,4,3,5] // [7,0,5,5,0]
*/

/*
Given a list of integers, write a function which iterates through every element and return list of lists of Lucas sequence as shown in example.
The elements of the list indicates how many Lucas numbers are in the corresponding sublist and the Lucas sequence will continue in the next sublist.
(Assume there is no negative integer in the list)

Lucas sequence:
Same as Fibonacchi sequence. Just the starting value is different
L0 = 2
L1 = 1
Ln = Ln-1 + Ln-2

example: lucasList [3,2,1,2] = [[2,1,3],[4,7],[11],[18,29]]
*/
/*
lucasgen :: Int -> Int
lucasgen 0 = 2
lucasgen 1 = 1

lucasgen num = lucasgen (num - 1) + lucasgen(num - 2)


lucasLs list = [lucasgen x\\x <- [0..sum(list)]]

DivideList :: [Int][Int] -> [[Int]]

DivideList biglist [] = []
DivideList biglist [x:xs] = [take x biglist] ++ DivideList (drop x biglist) xs


lucasList :: [Int] -> [[Int]]
lucasList ourlist = DivideList (lucasLs ourlist) ourlist

*/


//Start = lucasList [3,2,1,2] //[[2,1,3],[4,7],[11],[18,29]]
//Start = lucasList [] // []
//Start = lucasList [10,5] // [[2,1,3,4,7,11,18,29,47,76],[123,199,322,521,843]]