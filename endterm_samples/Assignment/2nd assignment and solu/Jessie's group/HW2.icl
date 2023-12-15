module HW2
import StdEnv

/*
	Given 2 lists of integer, main list and auxillary list. 
	Divide the main list into 2-D sublist according to the auxillary list.
	
	Ex: 
	[2,8,7,3,9,0,4,3,5,6] and [3, 4, 3]
	then results [[2,8,7],[3,9,0,4],[3,5,6]]
	
	There is one constraint to do this: 
	the sum of the elements of aux list has to be the same as the length of the main list, 
	otherwise this task will not work.
	So, if the above condition is not satisfied, then results the empty list for the insufficient list.
	Ex:
	[5,6,8,7,9,0,5,3] [5,6,7]
	then results [[5,6,8,7,9],[0,5,3],[]]
*/

//DivideList :: [Int] [Int] -> [[Int]]

//DivideList list [] = []
//DivideList list auxlist = [take (hd auxlist) list: DivideList (drop (hd auxlist) list ) (tl auxlist)]

//Start = DivideList [2,8,7,3,9,0,4,3,5,6] [3, 4, 3]	// [[2,8,7],[3,9,0,4],[3,5,6]]
//Start = DivideList [5,6,8,7,9,0,5,3] [5,6,7]			// [[5,6,8,7,9],[0,5,3],[]]
//Start = DivideList [] [4,3,2]							//	[[],[],[]]
//Start = DivideList [7,6,8,9,0,4,3] []					// []

/*
	Given a list and a positive integer 'n', and Generate a 2-D list in which each sublist are of the following form according to the given 'n' value.
	The given n value will be non-zero and positive.
	Ex:
	[2,8,7,3,9] and 2 
	then results [[2,8,7,3,9],[2,8,7],[2],[]] => next sublist = removing 2 elements from the previous sublist
	
	[2,8,7,3,9] and 1
	then results [[2,8,7,3,9],[2,8,7,3],[2,8,7],[2,8],[2],[]]
	
	[2,8,7,3,9] and 6
	then result [[2,8,7,3,9],[]]
*/

//ExpandList :: [Int] Int -> [[Int]]
//ExpandList [] num = [[]]
//ExpandList list num 
//|num>0 = [list: ExpandList( take (length list - num) list ) num ]

//Start = ExpandList [2,8,7,3,9] 2		// [[2,8,7,3,9],[2,8,7],[2],[]]
//Start = ExpandList [2,8,7,3,9] 1		// [[2,8,7,3,9],[2,8,7,3],[2,8,7],[2,8],[2],[]]
//Start = ExpandList [5,6,7,4] 3		// [[5,6,7,4],[5],[]]
//Start = ExpandList [2,8,7,3,9] 6		// [[2,8,7,3,9],[]]
//Start = ExpandList [] 2				// [[]]
