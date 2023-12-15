module endretake_solved

import StdEnv

/* 1. Records with multiple tasks - 30 points. */
:: Movie = {title::String,casts::[Cast],year::Int,rating::Real,country::String}
:: Gender = Male | Female
:: Cast = {name::String,gender::Gender}

cast1 = { name="Jackie Chan", gender=Male}
cast2 = { name="Jet Li", gender=Male}
cast3 = { name="Millie Bobby Brown", gender=Female}
cast4 = { name="Chris Hemsworth", gender=Male}
cast5 = { name="Zendaya", gender=Female}
cast6 = { name="Emma Stone", gender=Female}
cast7 = { name="Emma Watson", gender=Female}
cast8 = { name="Sandra Bullock", gender=Female}
cast9 = { name="Chris Evans", gender=Male}
cast10 = { name="Tom Holland", gender=Male}
cast11 = { name="Tobey Maguire", gender=Male}
cast12 = { name="Bae Suzy", gender=Female}
cast13 = { name="Park Seo Joon", gender=Male}

movie1 = { title="MOVIE I.", casts=[cast1,cast6,cast10], year=2019, rating=8.5, country="USA"}
movie2 = { title="MOVIE II.", casts=[cast4,cast8,cast7], year=2020, rating=8.0, country="Spain"}
movie3 = { title="MOVIE III.", casts=[cast13,cast12,cast9], year=2019, rating=9.0, country="Korea"}
movie4 = { title="MOVIE IV.", casts=[cast3,cast11,cast8], year=2021, rating=7.5, country="India"}
movie5 = { title="MOVIE V.", casts=[cast2,cast4,cast10], year=2022, rating=7.4, country="Hungary"}
movie6 = { title="MOVIE VI.", casts=[cast3,cast5,cast8], year=2022, rating=7.4, country="Hungary"}

movieList = [movie1,movie2,movie3,movie4,movie5,movie6]
castList = [cast1,cast2,cast3,cast4,cast5,cast6,cast7,cast8,cast9,cast10,cast11,cast12,cast13]


/* 1.1. List of movies by cast - 5 points
Given the list of movies and a cast name (String). 
Find the list of movie titles in which the given cast participated. */

listByCast :: [Movie] String -> [String]
listByCast mList cName = [m.title \\ m <- mList | isMember cName (map (\x = x.name) m.casts)]

//Start = listByCast movieList "Sandra Bullock" // ["MOVIE II.","MOVIE IV.","MOVIE VI."]
//Start = listByCast movieList "Tom Holland"	// ["MOVIE I.","MOVIE V."]
//Start = listByCast movieList "Millie Bobby Brown" // ["MOVIE IV.","MOVIE VI."]


/* 1.2. Sorting - 5 points
Sort the movies in decreased order of the ratings of movies, 
and keep only their titles. */

sortRate :: [Movie] -> [String]
sortRate list = [t.title \\ t <- (sortBy (\a b = a.rating > b.rating) list)]

//Start = sortRate movieList 
// ["MOVIE III.","MOVIE I.","MOVIE II.","MOVIE IV.","MOVIE V.","MOVIE VI."]


/* 1.3. Country of min ratings - 5 points
Find the years of the movies with minimum rating. */

minMovie :: [Movie] -> [Int]
minMovie list = [t.year \\ t<-(filter (\x = (x.rating == y.rating)) list)]
where y = last (sortBy (\a b = a.rating > b.rating) list)

//Start = minMovie movieList // [2022,2022]


/* 1.4. Movies per year - 5 points
Given a year and a country, find the list of movie titles which 
are produced in that year by the given country. */

listByYearCountry :: [Movie] Int String -> [String]
listByYearCountry list y c = map (\x = x.title) (filter (\x = (x.year == y) && (x.country == c)) list)

//Start = listByYearCountry movieList 2022 "Hungary" // ["MOVIE V.","MOVIE VI."]
//Start = listByYearCountry movieList 2019 "Korea" // ["MOVIE III."]


/* 1.5. Instance and ratio - 5 points
Write an == instance for Gender, then for a list of casts
return as a Real value the ratio of Male/Female casts. */

instance == Gender
where
	(==) Male Male = True
	(==) Female Female = True
	(==) _ _ = False
	
//Start = cast8.gender == cast12.gender // True
//Start = cast8.gender == cast9.gender // False

gRatio :: [Cast] -> Real
gRatio list = toReal(sum[1 \\ x <- list | x.gender == Male])/toReal(sum[1 \\ x <- list | x.gender == Female])

//Start = gRatio castList // 1.16666666666667


/* 1.6. Cast names - 5 points
For a given list of movies, find the names of the casts. */

listName :: [Movie] -> [String]
listName x = flatten [f m \\ m <- x] 
where f m = [x.name \\ x <- m.casts]

//Start = listName [movie1, movie3] 
// ["Jackie Chan","Emma Stone","Tom Holland","Park Seo Joon","Bae Suzy","Chris Evans"]
//Start = listName [movie5, movie6]
// ["Jet Li","Chris Hemsworth","Tom Holland","Millie Bobby Brown","Zendaya","Sandra Bullock"]


/* 2. Remove vowels - 10 points
Given a string, remove all the english vowels. */

removeVowel :: String -> String
removeVowel str = toString [x \\ x <-: str | not (isMember x ['a','e','i','o','u','A','E','I','O','U'])]

//Start = removeVowel "Hello World!" // "Hll Wrld!"
//Start = removeVowel "XaeiouAEIOU" // "X"
//Start = removeVowel "HELLO WoRLD" // "HLL WRLD"


/* 3. Trim whitespaces - 10 points
Given a string, trim leading and ending whitespaces. */

trimWhiteSpace :: String -> String
trimWhiteSpace str = toString (reverse (dropWhile (\x = x == ' ') (reverse(dropWhile (\x =( x == ' ')) [x \\ x <-: str]))))

//Start = trimWhiteSpace "  Hello World!       " // "Hello World!"
//Start = trimWhiteSpace " H e l l o   " // "H e l l o"
//Start = trimWhiteSpace "       " // ""
//Start = trimWhiteSpace "     He  l  l o     " // "He  l  l o"


:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
         
treeOne = Node 10 (Node 8 (Node 4 (Node 10 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 7 (Node 10 Leaf Leaf) (Node 10 Leaf (Node 10 Leaf Leaf)))
treeTwo = Node 10 (Node 2 (Node 4 (Node 9 Leaf Leaf) Leaf) (Node 3 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 3 Leaf Leaf))) 
treeThree = Node "haha" (Node "hehe" (Node "hoho" (Node "heyhey" Leaf Leaf) Leaf) (Node "haha" Leaf Leaf)) (Node "hey" (Node "hmm" Leaf Leaf) (Node "haha" Leaf (Node "hmm!" Leaf Leaf))) 
treeFour = Node 10 (Node 5 (Node 4 (Node 2 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 14 (Node 13 Leaf Leaf) (Node 15 Leaf (Node 18 Leaf Leaf)))
treeFive = Node 10 (Node 5 (Node 14 (Node 2 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 14 (Node 13 Leaf Leaf) (Node 15 Leaf (Node 18 Leaf Leaf)))
treeSix = Node 10 (Node 5 (Node 4 (Node 6 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 4 (Node 13 Leaf Leaf) (Node 20 Leaf (Node 18 Leaf Leaf)))
treeSeven = Node 10 (Node 5 (Node 14 (Node 2 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 14 (Node 13 Leaf Leaf) (Node 15 Leaf (Node 18 Leaf Leaf)))


/* 4. Value counting - 10 points
Given a tree, and a value, count the nodes that have that value. */

nodeCountByValue :: (Tree a) a -> Int | Eq a
nodeCountByValue Leaf _ = 0
nodeCountByValue (Node x left right) value
| x == value = 1 + nodeCountByValue left value + nodeCountByValue right value
= nodeCountByValue left value + nodeCountByValue right value

//Start = nodeCountByValue treeOne 10 // 5
//Start = nodeCountByValue treeTwo 3 // 3
//Start = nodeCountByValue treeThree "haha" // 3


/* 5. Check BST - 10 points
Given a tree, check if the tree is a binary search tree. */

NodeExtract :: (Tree a) -> a
NodeExtract (Node x left right) = x

bsTree :: (Tree a) -> Bool | Ord a
bsTree Leaf = True
bsTree (Node x Leaf Leaf) = True
bsTree (Node x Leaf right) = x < (NodeExtract right) && bsTree right
bsTree (Node x left Leaf) = (NodeExtract left) < x && bsTree left
bsTree (Node x left right) = (NodeExtract left) < x && x < (NodeExtract right) && bsTree left && bsTree right

//Start = bsTree treeFour // True
//Start = bsTree treeFive // False


/* 6. Count parents - 10 points
Given a tree of integers, count the nodes for which the 
node value is greater than the sum of its both children.
All the node in the tree are positive numbers. */

NodeExtractInt :: (Tree Int) -> Int
NodeExtractInt Leaf = 0
NodeExtractInt (Node x left right) = x

countHeap :: (Tree Int) -> Int
countHeap Leaf = 0
countHeap (Node x left right) 
| x > ((NodeExtractInt left) + (NodeExtractInt right)) = 1 + countHeap left + countHeap right
= countHeap left + countHeap right

//Start = countHeap treeSix // 6
//Start = countHeap treeSeven // 5


/* 7. Same trees - 10 points
Given two binary trees of integers, write a function to check 
if they are the same or not (structurally identical, 
and the nodes have the same value). */

getNode :: (Tree Int) -> Int
getNode (Node x l r) = x 

getL :: (Tree Int) -> ( Tree Int)
getL (Node  x l r) = l

getR :: (Tree Int) -> ( Tree Int)
getR ( Node  x l r) = r

isLeaf :: (Tree Int) -> Bool 
isLeaf Leaf = True 
isLeaf _  = False

isSameTree :: (Tree Int) (Tree Int) -> Bool 
isSameTree t1 t2 
| isLeaf t1 && isLeaf t2  = True 
| isLeaf t1 || isLeaf t2 || getNode t1 <> getNode t2 = False
=  isSameTree (getL t1) (getL t2) && isSameTree (getR t1) (getR t2)

//Start = isSameTree treeOne treeOne // True
//Start = isSameTree treeTwo treeTwo // True
//Start = isSameTree treeOne treeTwo // False
//Start = isSameTree treeFive treeFour // False


/* 8. Class - 10 points
Write a class 'UnionSet' for any type a and declare the 'U' binary operator 
which takes 2 lists of any type a and returns a list of any type a elements.
Write an instance to define 'U' operator for the Int type, 
which is the union of its given arguments in sorted order. */

class UnionSet a
where
	(U) :: [a] [a] -> [a]

instance UnionSet Int
where
	(U) list1 list2 = sort (removeDup (list1 ++ list2))
	
//Start = [2,3,4,5,6,7] U [6,7,2,4,6]	// [2,3,4,5,6,7]
//Start = [8,0,9,-2,-3] U [2,3,4,5] //[-3,-2,0,2,3,4,5,8,9]
//Start = [] U [5,4,6] // [4,5,6]


/* 9. Finding index - 10 points
Given an integer array and the integer list, which is an index list. 
Return the list of integers, which are the values at the indecses 
of the given array. Indexing starts at 0.
E.g: {23,45,346,6,7,73,92} [0,2,3,4,6] => [23,346,6,7,92]
If the given index is invalid, then no need to include it.
{23,45,346,6,7,73,92} [6,-10,1,0,8] => [92,45,23]
Index -10 and 8 are ignored because they are out of bound. */

subList :: {Int} [Int] -> [Int]
subList arr index = map (\x = arr.[x]) (filter (\x= (x < len) && (x >= 0)) index)
where
	len = length [x \\ x <-: arr]

//Start = subList {23,45,346,6,7,73,92} [0,2,3,4,5] // [23,346,6,7,73]
//Start = subList {-23,0,90,76,354,7} [1,0,-5,8] // [0,-23]
//Start = subList {} [3,5,-6,2] // []
//Start = subList {4,3,6,7,8} [-1,-2] // []


/* 10. Arithmetic sublists - 10 points
An integer sequence is called arithmetic if it consists of 
at least 3 elements, it is sorted, and the difference between 
each consecutive elements is the same. 
Given an integer array, return its arithmetic sublists, where 
elements must occur consecutively as in the original array.
E.g.: {1,2,3,4} -> [1,2,3],[2,3,4],[1,2,3,4] arithmetic (1-2=2-3=3-4)
{2,5,9,12,15,18,30} -> [9,12,15],[12,15,18],[9,12,15,18] arithmetic
[2,5,9] is not arithmetic 2-5 <> 5-9  */

subarrs :: [Int] Int -> [[Int]]
subarrs list n
| length list >= n = [take n list] ++ subarrs (tl list) n
= []

isArithmetic :: [Int] -> Bool
isArithmetic [_, _] = True
isArithmetic [x,y:xs]
| x - y == y - (hd xs) = isArithmetic [y:xs]
= False

arithmeticSubArray :: {Int} -> {[Int]}
arithmeticSubArray arr = {x \\ x <- subs| isArithmetic x}
where 
	subs = flatten([subarrs (list arr) n \\ n <- [3..length(list arr)]])
	list arr = [a \\ a <-: arr]

//Start = arithmeticSubArray {1,2,3,4} 
// {[1,2,3],[2,3,4],[1,2,3,4]}
//Start = arithmeticSubArray {7,7,7,7,7} 
// {[7,7,7],[7,7,7],[7,7,7],[7,7,7,7],[7,7,7,7],[7,7,7,7,7]}
//Start = arithmeticSubArray {2,5,9,12,15,18,30}
// {[9,12,15],[12,15,18],[9,12,15,18]}


/* 11. Players - 10 points
You are given a player's infos of a certain Basketball team.
We want to know the order of excellence of players on given 
position, so that we can make a team for the upcoming tournament.
Implement the function sortPlayer such that returns the players 
names with their excellence in increasing order.
The excellence is computed like it takes the highest pointWon, 
if it is equal, sort by height, if equal, sort by health. */

:: Position = PointGuard | ShootingGuard | Center | PowerForward | SmallForward
:: Player = {n::String,position::Position,pointWon::Int,health::Int,height::Int}

players1 = {{n="John", position=Center,pointWon=50,health=7,height=186}, {n="Andy", position=Center,pointWon=55,health=8,height=190}, {n="Micheal", position=Center,pointWon=49,health=6,height=194}, {n="Tom", position=Center,pointWon=49,health=6,height=186},
			 {n="Jeremy", position=Center,pointWon=49,health=10,height=180}, {n="Jo", position=SmallForward,pointWon=50,health=7,height=186}}
players2 = {{n="John", position=Center,pointWon=50,health=7,height=186}, {n="Andy", position=ShootingGuard,pointWon=55,health=8,height=190}, {n="Micheal", position=PowerForward,pointWon=49,health=6,height=194}, {n="Tom", position=SmallForward,pointWon=49,health=6,height=186},
			 {n="Jeremy", position=PointGuard,pointWon=49,health=10,height=180}, {n="Jo", position=SmallForward,pointWon=50,health=7,height=186}}

instance == Position
where
	(==) PointGuard PointGuard = True
	(==) ShootingGuard ShootingGuard = True
	(==) Center Center = True
	(==) PowerForward PowerForward = True
	(==) SmallForward SmallForward = True
	(==) _ _ = False
	
aux :: Player Player -> Bool
aux p1 p2
|p1.pointWon == p2.pointWon && p1.height == p2.height = p1.health < p2.health
|p1.pointWon == p2.pointWon = p1.height < p2.height
= p1.pointWon < p2.pointWon

sortPlayer :: Position {Player} -> {String}
sortPlayer p a = {player.n \\ player <- (sortBy (aux) [x \\ x <-: a | x.position == p])}

Start = sortPlayer Center players1 // {"Jeremy","Tom","Micheal","John","Andy"}
//Start = sortPlayer SmallForward players2 // {"Tom","Jo"}
