module HW4Solved
import StdEnv

/*
write a function that takes a matrix as list of lists and returns the transpose of the matrix
eg :
Input : [[1,2,3],
         [4,5,6],
         [7,8,9]]
Output : [[1,4,7],
          [2,5,8],
          [3,6,9]]
If there is an empty list or a list of different length, return an empty list

***Needs to use atleast one higher order function***
*/

funTran :: [[a]] -> [[a]]
funTran [] = []
funTran [[]:_] = []
funTran x = [(map hd x) : funTran (map tl x)]

// Start = funTran [[1,2,3],[4,5,6],[7,8,9]] // [[1,4,7],[2,5,8],[3,6,9]]
//Start = funTran [[1,2],[3,4],[5,6]] // [[1,3,5],[2,4,6]]


/*
Write a function that takes two matrices and returns the sum of the two matrices
If the matrices are not of the same order, return an error message
eg :
Input : [[1,2,3],     [[1,2,3],
         [4,5,6],      [4,5,6],
         [7,8,9]]      [7,8,9]]
Output : [[2,4,6],
          [8,10,12],
          [14,16,18]]
Needs to use atleast one higher order function
*/

//converting the two lists of lists into a list of list of lists
OneBigList :: [[Int]] [[Int]] -> [[[Int]]]
OneBigList [] [] = []
OneBigList [x:xs] [y:ys] = [[x,y]] ++ OneBigList xs ys

Sumx :: [Int] [Int] -> [Int]
Sumx [] [] = []
Sumx [x:xs] [y:ys] 
|(length [x:xs] <> length [y:ys]) || isEmpty [x:xs] || isEmpty [y:ys] = abort "Error"
= [x+y] ++ Sumx xs ys

SumList :: [[Int]] -> [Int]
SumList x 
| (isEmpty (x!!0)) || (isEmpty (x!!1)) = abort "Error"
= Sumx (x!!0) (x!!1)


funSum :: [[Int]] [[Int]] -> [[Int]]
funSum [] [] = []
funSum x y
| (length x <> length y) || isEmpty x || isEmpty y = abort "Error"
 = map SumList (OneBigList x y)


// Start = funSum [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3],[4,5,6],[7,8,9]] // [[2,4,6],[8,10,12],[14,16,18]]
// Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] // [[2,4],[6,8],[10,12]]
// Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4]] // Error
// Start = funSum [[],[1,3]] [[1,2],[4,5]] // Error






