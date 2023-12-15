module HW4
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

//Consider whether you should use higher order functions or recursion.

/* 1. Given an integer list, replace each element with the product of every other element 
without using the division operator. There can be repetitive numbers.
Example:
Input:  [1, 2, 3, 4, 5]
Output: [120 (2*3*4*5), 60 (1*3*4*5), 40 (1*2*4*5), 30 (1*2*3*5), 24 (1*2*3*4)] */




prod :: [Int] -> Int
prod [] = 1
prod list = hd list * prod (tl list) 



//Start = prod [1,2,3,4]

splitter :: Int [Int] -> [Int]
 
splitter num list 
| num< length list =  [ prod( removeAt num list) ]++  splitter (num+1) list
| otherwise = []


//Start = removeAt 0 [1,2,3]

//Start = splitter 0 [1,2,3,4,5]


q1 :: [Int] -> [Int]
q1 [] =[]
q1 list = splitter 0 list

//Start = q1 [1, 2, 3, 4, 5] // [ 120, 60, 40, 30, 24 ]
//Start = q1 [5, 3, 4, 2, 6, 5] // [720,1200,900,1800,600,720]
//Start = q1 [] // []

/* 2. You are given two lists, spells and potions, where spells represents the strength of each spell
and potions represents the strength of each potion.
You are also given an integer success. A spell and potion pair is considered successful if the product of their strengths
 is at least success.

Return a list where ith element is the number of potions that will form a successful pair with the ith spell. 

Example: spells = [5,1,3], potions = [1,2,3,4,5], success = 7 
- 0th spell: 5 * [1,2,3,4,5] = [5,(10),(15),(20),(25)]. 4 pairs are successful.
- 1st spell: 1 * [1,2,3,4,5] = [1,2,3,4,5]. 0 pairs are successful.
- 2nd spell: 3 * [1,2,3,4,5] = [3,6,(9),(12),(15)]. 3 pairs are successful.
Thus, [4,0,3] is returned.
*/
product :: Int [Int] -> [Int]

product num [] = []
product num [x:xs] = [num*x] ++ product num xs

//Start = product 5 [1,2,3,5]

listproduct :: [Int] [Int] -> [[Int]]

listproduct [] list = []
listproduct [x:xs] list =   [product x list: listproduct xs list] 



//Start = listproduct [1,5,3] [2,4,5] 
/*counter:: Int -> Int 
counter n = n+1

comp :: [[Int]] Int -> [Int]
comp list num 
|


q2 :: [Int] [Int] Int -> [Int]
q2 [] list succ = []
q2 list [] succ = []
q2 list1 list2 num =
*/
q2 :: [Int] [Int] Int -> [Int]
q2 [] potions success = []
q2 spells [] success = []
q2 spells potions success =
    map (\s -> length (filter (\p -> s * p >= success) potions)) spells
  
//Start = q2 [5,1,3] [1,2,3,4,5] 7 // [4,0,3]
//Start = q2 [3,1,2] [8,5,8] 16 // [2,0,2]
//Start = q2 [] [3,4,2,4] 9 // []
//Start = q2 [3,4,2,1] [] 9 // []