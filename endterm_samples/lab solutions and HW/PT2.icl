module PT2
import StdEnv

// Write your Neptun code here:                      


/* Given 2 dimentional list of Reals, sum all the numbers inside sublist and only keep the numbers if the fraction part is equal to zero. 

Example [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] => (after summing) [4.0, 110.16] => )(result) [4]   */


//version1
check :: Real -> Bool
check num = toReal(toInt num) == num

pt3 :: [[Real]] -> [Real]
pt3 [] = []
pt3 list 
| check (sum (hd list)) = [sum (hd list)] ++ pt3 (tl list)
= pt3 (tl list)

//Start = pt3 [] //[]
//Start = pt32 [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] // [4]
//Start = pt3 [[1.2, 1.8, 3.9], [4.8, 7.9, 6.7], [6.9]] // []