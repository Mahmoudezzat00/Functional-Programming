module midterm1 

import StdEnv

// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students� regulation of E�tv�s Lor�nd University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student�s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.



/*1. List ends
 Given a list of lists, append to the end of every sublist 
 the sum and the length of the sublist
*/

append :: [[Int]] -> [[Int]]
append list = [x ++ [sum x,length x]\\ x<- list]


//Start = append [[1..5],[1..4],[],[5,6]]  // [[1,2,3,4,5,15,5],[1,2,3,4,10,4],[0,0],[5,6,11,2]]
//Start = append [[(-1),(-2)..(-10)],[12],[5]]  // [[-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-55,10],[12,12,1],[5,5,1]]
//Start = append []  // []

/* 2. Fractions
 
 Given a list of real numbers, keep only the fraction part of the number
*/

pred :: Real Real -> Real
pred n i 
| (i+1.0) > n = i
= pred n (i+1.0) 


frac :: Real -> Real
frac n 
| n<1.0 = n
= n - (pred n 0.0)

fraction :: [Real] -> [Real]
fraction list = map frac list

//Start = fraction [1.2,1.5,0.6] //[0.2,0.5,0.6]
//Start = fraction [1.25, 8.2115548896, 53.21,45.58,0.005] //[0.25,0.2115548896,0.21,0.58,0.00005]
//Start = fraction [] // []


/*3. Famous nums

 Given a list of integers, write a function which gets rid of the numbers that is occurring
 less than 5 times in the list.
*/

occ :: Int [Int] -> Int
occ x list = length (filter ((==) x) list)

fnum :: [Int] [Int] -> [Int]
fnum [] list = []
fnum [x:xs] list
| (occ x list ) >= 5 = [x] ++ fnum xs list
= fnum xs list


famousNum :: [Int] -> [Int]
famousNum list = fnum list list

//Start = famousNum [1,1,1,1,1,1,2,3,4,4,4,4,5,5,5,5,5] // [1,1,1,1,1,1,5,5,5,5,5]
//Start = famousNum [] // []
//Start = famousNum [1,2,3,4,5,6,1,1,1,2,2,2,2,1,1,5,10,3] // [1,2,1,1,1,2,2,2,2,1,1]




/*4. Search
 
 Implement a search algorithm that searches through a list for Int n and returns the value in the list before n. 
 If there is no value, or the list is empty, return -1. e.g., findPrev 5 [1,2,3,4,5,6] should return 4, 
 while findPrev 5 [0, 10, 20, 30] returns -1.
*/

findPrev :: Int [Int] -> Int 
findPrev n list 
| hd list == n = abort"-1"
= hd [(list !! (y-1)) \\ x<- list & y<- [0,1..] | x == n  ]

//Start = findPrev 5 [1,2,3,4,5,6] // 4
//Start = findPrev 1 [1,2,3,4,5,6] // -1
//Start = findPrev 1 [] // -1 

 

/* 5. Symmetric difference 

 Given two lists of integer numbers , return a sorted list containing the symmetric difference of the two lists; 
 The symmetric difference of two lists A and B is the list (A � B) U (B � A); 
 where A - B is The difference of two lists  defined as follows:  
 The List A-B consists of elements that are in A but not in B.
 And (U) the union of two lists is a list containing all the elements of A and B without duplicates 
*/

sDif :: [Int] [Int] -> [Int]
sDif x y = (getu x y) ++ (getu y x)

getu :: [Int] [Int] -> [Int]
getu [] list = []
getu [x:xs] list
| isMember x list = getu xs list
= [x] ++ (getu xs list)

symmetricDif :: [Int] [Int] -> [Int]
symmetricDif x y = sort (sDif x y)

//Start = symmetricDif  [1,2,3,4,5] [2,4,6] //  [1,3,5,6]
//Start = symmetricDif  [1..5] [1..10] // [6,7,8,9,10]
//Start = symmetricDif  [1..5] [] // [1,2,3,4,5]

/*6. Not N

 Given a list of integers and an integer N, 
 eliminate from the list elements that are positioned before N in the list and are not equal to N,
 then compute the biquadrate of the numbers left in the list.
*/

indexOf :: Int [Int] Int -> Int 
indexOf n [] i = -1
indexOf n [x:xs] i
| n == x = i 
= indexOf n xs (i+1)

notN :: Int [Int] -> [Int]
notN n list = map (\x = x^4) (take ind list)
where ind = indexOf n list 0 

//Start = notN 3 [1..5] // [1,16]
//Start = notN 3 [] // []
//Start = notN 6 [10,8..1] // [10000,4096]

/* 7.  Gap2 continued 

 Given a list of numbers, return True if the  
 the difference between two consecutive elements is always 2
 otherwise return False
*/

gap2C :: [Int] -> Bool
gap2C [] = False
gap2C list = list == (take len [1,3..])
where len = length list 

//Start = gap2C [1,3,5,7] // True
//Start = gap2C [1,3,5,7,9,11,13,15] // True
//Start = gap2C [1,5,8] // False
//Start = gap2C [] // False

/* 8. Good Lists
 Given the list of lists and a list of unique numbers. 
 Numbers that are given in this second unique number list are considered to be good numbers. 
 A List is considered good if at least half of its numbers are good. Count how many good lists 
 are in the given list of lists.

 Ex. If you are given [[1,2,3], [1,3,3,4,9,6], [3..6]]  and [1,2,3], good numbers are 1, 2 and 3. 
 First list [1,2,3] has 3 good numbers out of total 3 numbers, hence it is good. 
 Next one [1,3,3,4,9,6] has 3 good numbers (1,3,3) which is half of total length, hence it is a good one as well.
 Last list [3..6] has only one good number and is not a good list. Therefore, answer for this example is 2.
*/
count :: [Int] [Int] -> Int
count [] list = 0
count [x:xs] list 
| isMember x list = 1 + (count xs list)
= count xs list

glist :: [Int] [Int] -> Bool 
glist goodlist list = (count list goodlist) >= half 
where half = (length list) / 2

goodLists :: [[Int]] [Int] -> Int
goodLists lists good = length (filter ((==) True) (map (glist good) lists))

//Start = goodLists [[1,2,3], [1..6], [3..6]] [1,2,3] // 2
//Start = goodLists [[1], [1..6], [3,8,5]] [1,2,3,8] // 3
//Start = goodLists [[], [3,2,5], [1,1,2,2]] [1] // 2
//Start = goodLists [] [1,2,3] // 0


/*9. CoPrimes
 Given 2 numbers, check if they are co-prime.
 Numbers are called co-prime if they do not have
 common divisor.
*/
coPrimes :: Int Int -> Bool
coPrimes a b = (length ([x \\ x <- [2..a] | (a rem x) == 0 && (b rem x) == 0])) == 0

//Start = coPrimes 12 9 // False
//Start = coPrimes 12 12 // False
//Start = coPrimes 12 13 // True
//Start = coPrimes 5 7 // True


/* 10. Clean Sequence
 The Clean sequence is defined in following way:
 s(0) = a
 s(1) = b
 s(2) = c
 and for every k greater than 2:
 s(k) = ( s(k-1)*s(k-2) + s(k-3) ) rem 1000
 
 Given n, a, b and c - generate first n numbers from Clean sequence.
*/

cc :: Int Int Int Int -> [Int]
cc n a b c 
| n == 0 = []
= [f] ++ cc (n-1) b c f
where f = (a*b + c) rem 1000

clean :: Int Int Int Int -> [Int] 
clean n a b c 
| n < 4 = take n [a,b,c]
= [a,b,c] ++ (cc (n-3) a b c) 

//Start = clean 5 1 2 3 // [1,2,3,5,11]
//Start = clean 11 123 79 3 // [123,79,3,720,957,117,157,126,495,277,647]
//Start = clean 2 1 2 3 // [1,2]
//Start = clean 1 1 2 3 // [1]


//Start = map ((<) 5) [4,3,9,1,8,10,6]