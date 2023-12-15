module HW3
import StdEnv

/*
	Given a 2-D list and return the boolean and eliminate the multiple of 3 in each sublist and check if the length of all sublists are even.
	For ex: [[6,4,7,32,8,9],[7,8,9,0,8,3,12],[0,27,4,8,2]]
	After elimination of multiple of 3 from all sublist:
			[[4,7,32,8],[7,8,0,8],[0,4,8,2]]
	Check if all the sublist are of "Even" Length
			[[4,7,32,8],[7,8,0,8],[0,4,8,2]]
			 length 4, length 4, length 4	=> all the length are Even, then the result is True, otherwise, false.
	
*/

Elimination:: [[Int]] -> Bool
Elimination xs = isEven (sum (map length (EliminationSublist xs)))
 

EliminationSublist :: [[Int]]->[[Int]]
EliminationSublist xs=map(\ys->filter(\x->x rem 3 ==0)ys)xs 


//Start = Elimination [[6,4,7,32,8,9],[7,8,9,0,8,3,12],[0,27,4,8,2]]	// True
// Start = Elimination [[3,6,27,15],[2,4,7,9]]							//	False
// Start = Elimination	[]												// True
// Start = Elimination [[],[3,4,5,28]]									// False
	
/*
	Given a list of integer, and an integer, return the index of that integer inside the list.
	If there are duplicate element, return the index of the first found element.
	If there is no such element, return -1.
	Ex: [7,4,8,4,9] 9 => the result is 4.
		 0,1,2,3,4 (indices)
		 
	You are not allowed to use any built-in function.
*/


Search :: [Int] Int -> Int
Search xs y = go xs y 0

go :: [Int] Int Int -> Int
go [] _ _ = -1
go [x:xs] y i
| x == y    = i
= go xs y (i+1)

//Start = Search [7,4,8,4,9] 9			// 4
//Start = Search [9,2,8,3] 4			// -1
//Start = Search [] 2					// -1
//Start = Search [4,7,8,3,9,0,9,8] 8	// 2
	