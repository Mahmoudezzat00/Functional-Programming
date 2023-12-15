module HW6
import StdEnv

//Please name the file as your name and change the module name
//Below are three questions, you can choose two of them and solve to get a full score. But you can solve all three of them if you want.

/* You are given a list citations, where i th position number represents 
how many times the ith paper of the researcher has been cited. 
You have to return the researcher's h-index. (You can search from Wikipedia to learn more)
H-index is the maximum value of h such that at least h papers have
been cited at least h times.

for example: [3,0,6,1,5] means the researcher has 5 papers where
1st one is cited 3 times, 2nd one 0 times and so on.
The h-index for this researcher is 3 because 3 of the papers have been cited at least 3 times (3,5,6)
It cannot be 4 because there is no 4 papers that have been cited at least 4 times. */



hIndex :: [Int] -> Int
hIndex citations = hIndexHelper (sort (removeDup citations)) (length  (removeDup citations))


hIndexHelper :: [Int] Int -> Int
hIndexHelper citations h
| h == 0 || citations!!(h - 1) <= h = h
| otherwise = hIndexHelper citations (h - 1)

//Start = hIndex [3,0,6,1,5] // [0,1,3,5,6] -> [[0],[1],[3],[5],[6]]         ==3
//Start = hIndex [1,3,1] //[1,1,3] 1
//Start = hIndex [1,2,78,43,3,4,68,90] // 4

/* Reshape a matrix
There is a reshape function in MatLab that changes the dimenstion of a matrix with the given new dimension
and we want to implement this functionality in Clean. 
for example: Suppose we have a 1x20 matrix with values 1..20 and if we change the shape to 4x5 matrix result will be

1 5 9 13 17
2 6 10 14 18
3 7 11 15 19
4 8 12 16 20

As you can see the columns are filled first. See the start expressions for the input and expected output
P.S If you cannot solve like this then you can fill the rows first. It will be accepted, but first try to fill the columns first.
*/

null :: [a] -> Bool
null xs 
|isEmpty xs = True
| otherwise = False 

concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

reShape :: [[Int]] Int Int -> [[Int]]
reShape xs rows cols
|length flatList <> rows*cols = abort "The input is not correct"
|otherwise = splitRows flatList cols
    where flatList = concat xs

splitRows :: [a] Int -> [[a]]
splitRows xs n
|null xs = []
|otherwise = [row : splitRows rest n]
    where (row, rest) = splitAt n xs


//Start = reShape [[1..20]] 4 5 // [[1,5,9,13,17], [2,6,10,14,18], [3,7,11,15,19], [4,8,12,16,20]]
//Start = reShape [[1,2],[3,4]] 1 4 // [[1,2,3,4]]
//Start = reShape [[1,2],[3,4]] 2 4 // "The input is not correct" // the given matrix cannot be transformed to 2x4 matrix
//Start = reShape [[1,2,4,7,6,1], [2,9,4,1,7,3]] // [[1,9,7,7],[2,4,1,1], [2,4,6,3]]

/* Game of Life (https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)

The board is made up of MxN grid of cells where 1 is live 0 is dead
Each cell lives or dies based on the condition of its 8 neighboring cells.

1. Any live cell with fewer than two live neighbours dies of loneliness.
2. Any live cell with two or three live neighbours lives on to the next generation.
3. Any live cell with more than three live neighbours dies, because of overpopulation.

You have to return the next generation of the given grid of cells. */

//gameOfLife :: [[Int]] -> [[Int]]


//Start = gameOfLife [[0,1,0],[0,0,1],[1,1,1],[0,0,0]] // [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
//Start = gameOfLife [[1,1],[1,0]] // [[1,1], [1,1]]
