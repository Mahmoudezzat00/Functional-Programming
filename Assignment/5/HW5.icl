module HW5
import StdEnv

/// PLEASE NAME THE FILE AFTER YOUR NAME AND MAKE SURE TO CHANGE THE MODULE NAME! 

/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code. */

/* Given a list of distinct numbers, generate all the possible tuples such that
(a,b,c) fulfills a * 2 + b * 3 = c^2

For example given [2,3,4,6]. The answer would be (2,20,8)
Answer (20,8,8) is not valid as in one tuple the same number should not be used. */ 

tup :: [Int] -> [(Int, Int, Int)]
tup numbers   = [(a, b, c) \\ a <- numbers, b <- numbers, c <- numbers | a <> b && a <> c && b <> c && a * 2 + b * 3 == c * c]
//Start = tup [2, 20,4, 8] // [2,20,8]
//Start = tup [1,2,3,4,5,6,7,8,9,100,23,43,643,35,63,35,34,67] // [(2,7,5),(5,2,4),(6,23,9),(8,3,5),(23,1,7),(23,6,8)]

/* Given an list of integers, divide the list into two parts (left and right) that fulfills the following conditions.
1. Every element in left side should be less or equal to the right side
2. The original order of numbers should be preserved
3. Left side's length should be as small as possible.   */
/*minimum :: [Int] -> Int
minimum list = hd(sort list)
 	 	
div :: [Int] -> [[Int]]
div [] = [[]]
div xs = [left, right]
  where
    right = drop (length left) xs
    left = takeWhile (\x -> x <= minimum right) xs*/
    
minimum :: [Int] -> Int
minimum list = hd(sort list)

divideList :: [Int] -> [[Int]]
divideList xs = [[x \\ x <- xs | x <= minimum right], right]
  where
    right = dropWhile (<=minimum xs) xs
    
 


Start = divideList [1,1,1,0,6,12] // [[1,1,1,0], [6,12]]
//Start = div [5,0,3,8,6] // [[5,0,3], [8,6]]
//Start = div [1,2,3,4] // [[1], [2,3,4]]
//Start = div [] // []


