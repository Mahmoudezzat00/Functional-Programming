module HW2
import StdEnv

//Please write your neptun code here: FOZLQN
/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code. */

// Given a list, find all distinct combination of length 2. Repetition of elements is allowed.

/*
comb :: [Int] -> [[Int]]

comb [] = []
comb list =  [ [x,y] \\ x <- list, y <- list | x<= y ]
*/
//Start = comb [1,2,3] // [[1,1],[1,2],[1,3],[2,2],[2,3], [3,3]]
//Start = comb [1] // []
//Start = comb [1,2,3,4,5] // [[1,1],[1,2],[1,3],[1,4],[1,5],[2,2],[2,3],[2,4],[2,5],[3,3],[3,4],[3,5],[4,4],[4,5], [5,5]]

/*
Given a list consisting of 4 real values (first two values are one of the vertices of a rectangle x and y coordinate,
third is the rectangle's width and the fourth is the rectangle' height (width and height can be negative too). For example,
in case of  x =1, y=5, width=6, height=-2,
the bottom left vertex coordinates will be 1 and 3.
Define topLeft, topRight , bottomRight, and bottomLeft functions returning
the corresponding vertex of the rectangle as a list consisting of  x , y represinting the point of the resulting function.
*/


topLeft :: [Real] -> [Real]
topLeft [x, y, w ,h]
    | w >= 0.0 && h >= 0.0 = [x, y+h]
    | w >= 0.0 && h < 0.0 = [x, y]
    | w < 0.0 && h >= 0.0 = [x+w, y+h]
    | otherwise = [x+w, y]

topRight :: [Real] -> [Real]
topRight [x, y, w ,h]
    | w >= 0.0 && h >= 0.0 = [x+w, y+h]
    | w >= 0.0 && h < 0.0 = [x+w, y]
    | w < 0.0 && h >= 0.0 = [x, y+h]
    | otherwise = [x, y]

bottomRight :: [Real] -> [Real]
bottomRight [x, y, w ,h]
    | w >= 0.0 && h >= 0.0 = [x+w, y]
    | w >= 0.0 && h < 0.0 = [x+w, y+h]
    | w < 0.0 && h >= 0.0 = [x, y]
    | otherwise = [x, y+h]

bottomLeft :: [Real] -> [Real]
bottomLeft [x, y, w ,h]
    | w >= 0.0 && h >= 0.0 = [x, y]
    | w >= 0.0 && h < 0.0 = [x, y+h]
    | w < 0.0 && h >= 0.0 = [x+w, y]
    | otherwise = [x+w, y+h]


 
//Start = topLeft[3.0 , 5.0 , 1.0 , -7.0] ++  topRight[3.0 , 5.0 , 1.0 , -7.0] ++ bottomLeft [3.0 , 5.0 , 1.0 , -7.0] ++ bottomRight[3.0 , 5.0 , 1.0 , -7.0]  
          //[3.0 ,5.0 , 4.0 , 5.0  , 3.0 , -2.0 ,  4.0 ,  -2.0  ]
Start = topLeft[2.0,8.0,-4.0,-7.0] ++  topRight[2.0,8.0,-4.0,-7.0] ++ bottomLeft[2.0,8.0,-4.0,-7.0] ++ bottomRight[2.0,8.0,-4.0,-7.0]
         // [-2.0 ,8.0 ,2.0 ,8.0 ,-2.0 ,1.0 ,2.0 ,1.0]
//Start = topLeft[1.0,6.0,4.0,2.0] ++  topRight[1.0,6.0,4.0,2.0] ++ bottomLeft[1.0,6.0,4.0,2.0] ++ bottomRight[1.0,6.0,4.0,2.0]
		  // [1.0 ,8.0 ,5.0 ,8.0 ,1.0 ,6.0 ,5.0 ,6.0]
