module ex1
import StdEnv

//1. generate a list from 1 to x 

listgen :: Int -> [Int]

listgen x 

| x <= 0 = []
| x == 1 = [1]
= listgen(x-1) ++ [x]

//Start = listgen 10

// 2. compute the sum of each subset 
//  [ [1,2] , [3,4,5] , [6,5,9,7] ,[], [8] ] -> [3,12,27,0,8]


//sum of a list using a built in function 
//Start = sum [1,2,3,4]

list :: [[Int]] -> [Int]
list x
| isEmpty x = []
| isEmpty(hd x) = [0: list(tl x)] 
= [sum(hd x) : list(tl x)]


list2 :: [[Int]] -> [Int]
list2 x = map sum (x )

//Start = list [ [1,2] , [3,4,5] , [6,5,9,7] ,[], [8] ]
//Start = list2 [ [1,2] , [3,4,5] , [6,5,9,7] ,[], [8] ]


//3. Insert x as the first element in every sublist 	
// if the sublist is empty then x will be the only elemnt in this list 

// [[1,2], [4,5], [], [1,5,4,7]] 9 -> [[9,1,2]. [9,4,5], [9], [9,1,5,4,7]]


insx :: [[Int]] Int -> [[Int]]


insx [] _ = []
insx biglist insme = [[insme : (hd biglist)] : insx(tl biglist) insme ]

//Start = insx [[1,2], [4,5],[], [1,5,4,7]] 9

//insx2 biglist insme = map (\sublist = [insme:sublist]) biglist
//insx3 biglist insme = [[insme:sublist]\\sublist<-biglist]
