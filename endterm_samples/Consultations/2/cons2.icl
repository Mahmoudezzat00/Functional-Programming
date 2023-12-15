module cons2
import StdEnv 
/*1-
    * Given a positive integer number, check if the given number is a Disarium number or not.
    * A Disarium number is a number defined by the following process :
    * Sum of its digits powered with their respective position is equal to the original number.
    * For example 135 is a Disarium number :
    * As 1^1 + 3^2 + 5^3 = 135
*/
// V1(higher order functions)
toDigits :: Int -> [Int]
toDigits x 
| x < 10 = [x  ]
=   toDigits (x / 10) ++ [(x rem 10) ] 
// [1,2,3]
// toDigits 123 => [1,2,3]
// !!1
splitIntoList :: Int -> [Int]
splitIntoList n 
| n < 10 = [n] 
= [(n rem 10)] ++ splitIntoList (n/10) 

// [3,2,1]
// Start = reverse (splitIntoList 123) // [1,2,3]
isDisariumNum :: Int ->  Bool 
isDisariumNum n = calculate n == n 

calculate :: Int -> Int 
calculate n  
| n < 10 = n
= (n rem 10) ^ size (toString n) + calculate (n/10)

sumOfOddDigitsInAnumber :: Int -> Int
sumOfOddDigitsInAnumber n 
| isOdd n && n < 10 = n 
| isEven n && n < 10 = 0 
| isOdd (n rem 10) = (n rem 10) + sumOfOddDigitsInAnumber (n/10)   
= sumOfOddDigitsInAnumber (n / 10 )
// Start = sumOfOddDigitsInAnumber 234
// Start = sumOfOddDigitsInAnumber 2345 // 8
// sumOfOddDigitsInAnumber 2345 => 5 + sumOfOddDigitsInAnumber(234) 
// 2) 5 + sumOfOddDigitsInAnumber(234) => sumOfOddDigitsInAnumber(234) =>  sumOfOddDigitsInAnumber (23)
// 3) 5 + sumOfOddDigitsInAnumber(23) => 3 +  sumOfOddDigitsInAnumber(2)
// 4) 5 + 3 + sumOfOddDigitsInAnumber(2)   
// 4) 5 + 3 + 0   = 8 



// 5^3  + 3^2 + 1^1 = 135
// Start = calculate  => 
// 1) calculate  135  => (135 rem 10) ^  3 +  calculate (13) => 5 ^ 3 + calculate (13)
// 2)  5 ^ 3 + calculate (13) => calculate (13) => (13 rem 10) ^ 2 + calculate (1) => 3 ^ 2 + calculate (1)
// 3) 5 ^ 3 +  3 ^ 2 + calculate (1) => calculate (1) => 1 => 5 ^ 3 +  3 ^ 2 + 1 = 135  


// isDisariumNum :: Int ->  Bool 
// isDisariumNum a =  sum [(toInt x)^i \\ x <- (toDigits a ) &  i <- [1..]]  == a  
 
//V2
// isDisariumNumV2 :: Int ->  Bool 
// isDisariumNumV2 n = (isDisariumNumV2Aux n (size(toString n)) )== n
// isDisariumNumV2Aux :: Int Int -> Int
// isDisariumNumV2Aux n counter
// | counter == 1 = n
// = ((n rem 10) ^ counter) +  isDisariumNumV2Aux (n / 10) (counter - 1)

// Start =  isDisariumNumV2 135 // True
// Start =  isDisariumNumV2 598 // True
// Start =  isDisariumNumV2 518 // True
// Start =  isDisariumNumV2 220 // False
// Start =  isDisariumNumV2 110 // False

/*2- 
    * Given a list of positive integer numbers, return a list contains the 'Harshad' numbers in the given list.
    * a harshad number is an integer that is divisible by the sum of its digits when written in that base.
    * Examples:
                1-  Number 200 is a Harshad Number because the sum of digits 2 , 0 and 0 is 2 and 200 is divisible by 2. 
                2- Number 171 is a Harshad Number because the sum of digits 1 , 7 and 1 is 9 and 171 is divisible by 9.
*/
// V1
harshadNums :: [Int] ->  [Int]
harshadNums  [] = [] 
harshadNums [x:xs]  
| isHarshadNum x = [x] ++ harshadNums xs 
= harshadNums xs  
// harshadNums [8, 9, 10, 12, 18, 20, 21, 24, 27, 30] = [8] ++   harshadNums [9, 10, 12, 18, 20, 21, 24, 27, 30]
//             x   - - -- - - xs - - - 

//  harshadNums [9, 10, 12, 18, 20, 21, 24, 27, 30] = [9] ++  harshadNums  [10, 12, 18, 20, 21, 24, 27, 30] 
//               x  - - - -- xs  - 

// [8] ++  [9] = [8,9,....] 

isHarshadNum :: Int -> Bool
isHarshadNum n = n rem (sumOfDigits n) == 0  
sumOfDigits :: Int -> Int 
sumOfDigits n 
|  n < 10 = n
= (n rem 10) + sumOfDigits (n / 10 ) 



// Start = harshadNums ([8, 9, 10, 12, 18, 20, 21, 24, 27, 30] ++ [13..17]) //   [8, 9, 10, 12, 18, 20, 21, 24, 27, 30] 
// Start = harshadNums ([31..35] ++ [36, 17,40, 42, 45, 13, 48, 50, 54, 11, 60, 63]) // [36, 40, 42, 45, 48, 50, 54, 60, 63]
// Start = harshadNums [] // []
// V2 
harshadNumsV2 :: [Int] ->  [Int]
harshadNumsV2 [] = [] 
harshadNumsV2 [x:xs]
| isHarshadNum x = [x] ++ harshadNumsV2 xs
= harshadNumsV2 xs


/*3- 
    *Given a list of integer numbers, return all the LEADERS in the list.
    A number is leader if it is greater than all the elements to its right side.
    Example:
        [10,9,14,23,15,0,9] -> [23,15,9]
        23 is greater than all the numbers to its right (15,0,9).  
        15 is greater than all the numbers to its right (0,9).  
        9 there is no numbers in its right.  
*/
isLeader :: Int [Int] -> Bool
isLeader x list = and [a < x \\ a <- list]

leaders :: [Int] -> [Int]
leaders [] = [] 
leaders [x:xs]
| isLeader x xs = [x] ++  leaders xs
= leaders xs
// Start = leaders  [10,9,14,23,15,0,9] // [23,15,9]
// Start = leaders  [1..10] // [10]
// Start = leaders  [10,9..1] // [10,9,8,7,6,5,4,3,2,1]
// Start = leaders  [7,8,10,9,5,3,6,4] // [10,9,6,4]
// Start = leaders  [] // []

/* Switcherr
 * Complete the function Switcher that takes two integer numbers, start and target.
 * The function Switcher returns the amount of digit switches that have to be made in the start number
 * such that the end product is equal to the target 
 * e.g: Switcher 12132 21123 -> 2                    
 * we had to switch 12 and 32 in the start number in order to get the target number
 * Note: it is guranteed that a number of switches in the start numbere will result in the target number 
*/

// Switcher :: Int Int -> Int 
// Switcher start target =  length ( filter ((<)0) ((filter ((<>)0) [b-a \\ a<-(numTo start)& b<-(numTo target)])))

// Start = Switcher 12132 21123 // 2
// Start = Switcher 99428123 98439122 // 2
// Start = Switcher 11112111 12111111 // 1


/*Empty parking lots
 * You are given an integer number representing the length of a parking lot, and a list of the occupied parking spots 
 * Count the number of the empty parking spots that fit for a truck, if a truck takes two consecutive empty parking spots to be able to park properly 
 * e.g: truckParker 10 [2,4,6,9,10] -> 1
 * The truck can only park between 6 and 9 at position 7
*/
countConseq :: [Int] -> Int
countConseq [] = 0
countConseq [x] = 0
countConseq [x,y:xs] 
|y-x == 1 = 1 + countConseq [y:xs]
=countConseq [y:xs]
truckParker :: Int [Int] -> Int
truckParker n list =  countConseq (removeMembers [a \\ a<-[1..n]] list)
//Start = truckParker 10 [2,4,6,9,10]   //  1
//Start = truckParker 100 [1,4..100] //  33
//Start = truckParker 2 [2] // 0
//Start = truckParker 2 [] // 1

/* Even conditions
 * Write a function which takes a list of lists
 * and returns the biggest even sum between all of the sums 
 * example: [[1,2,3],[],[10,1],[2,2]] -> 6 because the sum
 * of [1,2,3] is 6 and it is biggest even between the other sums
 * 0, 11 and 4
 
 * Note: sum of empty list is 0
 * Note: if there is no even sums, return -1
*/
evenConds:: [[Int]] -> Int
evenConds [] = 0 
evenConds listOfLists = maxList (filterEvens (sumOfSublists listOfLists)) // [6,0,4] 
sumOfSublists :: [[Int]] -> [Int]
sumOfSublists [] = [] 
sumOfSublists [x:xs] = [(sum x)] ++ sumOfSublists xs

filterEvens :: [Int] -> [Int] 
filterEvens [] = [] 
filterEvens [x:xs]
| isEven x = [x] ++ filterEvens xs 
= filterEvens xs 
filter (isEven) list
// Start = sumOfSublists [[1,2,3],[],[10,1],[2,2]] // [6,0,11,4]  
// Start = filterEvens [6,0,11,4]    // [6,0,4]  
// Start = evenConds[[1,2,3],[],[10,1],[2,2]]     // [6,0,4]  
// maxList [1,2,3] -> 3 
// minList -> 1

// Start = evenConds [[3,3,3],[],[10,5,1],[15,2]] //16
// Start = evenConds [[1,1], [9,9], [100,1]] //18
// Start = evenConds [[90,3,6],[71,52,12,1],[53,25]] //136
// Start = evenConds [] //0

listToInt  :: [Int] -> Int 
listToInt [] = 0
listToInt list = toInt (listToString  list) 

listToString :: [Int] -> String
listToString [] = ""  
listToString [x:xs] =  (toString x) +++ listToString xs   
listToString [1,2,3] = "1" +++ listToString [2,3] 
                --- 
    `          x xs
"1" +++ listToString [2,3]  =>  listToString [2,3] = "2" +++ listToString [3] 

"1" +++ "2" +++ listToString [3] 
"12" +++ "3" = "123"
Start = listToInt [1,2,3] // -> toInt "123" -> 123 

/* Bigger than average 
 * 
 * given a list of lists of integers and a number,
 * write a function which returns the lists where
 * whether the difference between the maximum
 * and the average is bigger than or equal to a given
 * number.
*/

BTavg :: [[Int]] Int -> [[Int]]
BTavg list num = filter (\x = (maxList x) - (avg x) >= num) list
//Start = BTavg [[1,2,4], [1,2]] 2 // [[1,2,4]]
//Start = BTavg [[1]] 1 // []
//Start = BTavg [[1..10], [20..30], [1..100]] 10 // [[1..100]]