module HW_extra
import StdEnv

// Please write your name and neptun code
//Change the name of the file and module after your name

//Each question is 100 points worth

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

/* Given a binary tree return the sum of each level's max numbers 
e.g     3
	/ 		\
	8   	19
   / \      /  \
   -6 9     0   1
For this tree you will return 3 + 19 + 9 = 31 */
max :: Int Int -> Int 
max a b 
| a>b = a 
= b
maxchildlvl :: (Tree Int) (Tree Int) -> Int
maxchildlvl Leaf Leaf = 0
maxchildlvl (Node x left right) (Node y lf rt)
| x>y = x + (max(maxchildlvl left right) (maxchildlvl lf rt))
|otherwise = y + (max (maxchildlvl left right) (maxchildlvl lf rt))

maxLevel :: (Tree Int) -> Int
maxLevel Leaf = 0
maxLevel (Node x left right) = x + maxchildlvl left right 

//Start = maxLevel (Node 3 (Node 8 (Node -6 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 19 (Node 0 Leaf Leaf) (Node 1 Leaf Leaf))) // 31
//Start = maxLevel (Node 2 (Node 1 Leaf Leaf)(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))) // 7
//Start = maxLevel (Node 4 (Node 0 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) (Node 6 (Node 0 Leaf Leaf) (Node 7 Leaf Leaf))) // 17


//2-Given an array of Libraries return the library for which the  total sum of the  sum of  impTopics of all the books in the library is the highest in the given array of libraries .


/*
the sum of impTopics of the two books  in lib1 is  3 + 4 + 5 + 6 + 20 + 50 + 10 + 10 + 10 =  118  
the sum of impTopics in lib2 =  3 + 4 + 5 + 6 = 18 
 
hence libraryWithHighestSum {lib1 , lib2} is lib1 because the total sum of the  sum of impTopics of its books is the biggest in the given array.


(Library "lib1" {(Book "b1" 4 {3,4,5,6}),(Book "b2" 6 {20,3,4,3,0,100}),(Book "b3" 1 {0})} 3) 
(Library "lib2" {(Book "b1" 4 {3,4,5,6}),(Book "b2" 6 {20,3,4,3,0,100}),(Book "b3" 1 {0}),(Book "b4" 8 {1,2,3,4,5,6,7,8}),(Book "b5" 5 {20,50,10,10,10})} 5)
(Library "lib4" {(Book "b5" 5 {20,50,10,10,10}),(Book "b3" 1 {0}),(Book "b2" 6 {20,3,4,3,0,100})} 3)
(Library "lib5" {(Book "b1" 4 {3,4,5,6})} 1)
*/ 
:: Library = {nameee :: String , books :: {Book} , numberOfBooks :: Int}
:: Book =  {
               name :: String ,
               numberOfChapters :: Int,
               impTopics :: {Int}    // containing numbers indicating the number of important topics in each chapter of the book.
             }

b1 = {name = "b1" , numberOfChapters = 4 , impTopics = {3,4,5,6}}
b2 = {name = "b2" , numberOfChapters = 6 , impTopics = {20,3,4,3,0,100}}
b3 = {name = "b3" , numberOfChapters = 1 , impTopics = {0}}
b4 = {name = "b4" , numberOfChapters = 8 , impTopics = {1,2,3,4,5,6,7,8}}
b5 = {name = "b5" , numberOfChapters = 5 , impTopics = {20,50,10,10,10}}

lib1  = {nameee = "lib1" , books = {b1,b2,b3} , numberOfBooks = 3}
lib2  = {nameee = "lib2" , books = {b1,b2,b3,b4,b5} , numberOfBooks = 5}
lib3  = {nameee = "lib3" , books = {b1,b5} , numberOfBooks = 2}
lib4  = {nameee = "lib4" , books = {b2,b3,b5} , numberOfBooks = 3}
lib5  = {nameee = "lib5" , books = {b1} , numberOfBooks = 1}

imptopicsperbook :: Book -> Int
imptopicsperbook book = sum [x \\ x <-: book.impTopics]

imptopicsperlibrary :: Library -> Int
imptopicsperlibrary lib = sum [imptopicsperbook x \\ x <-: lib.books]

libraryWithHighestSum :: {Library} -> Library
libraryWithHighestSum arr 
|size arr == 0 = abort"No library"
|otherwise= hd [x \\ x <-: arr | imptopicsperlibrary x == highestnumoftopics]
  where highestnumoftopics = maxList [imptopicsperlibrary x \\ x <-: arr]


// lib1= 148/lib2=284/lib3=118 /lib4=230/lib5=18 

//Start = libraryWithHighestSum {lib1,lib3,lib5} //lib1
//Start = libraryWithHighestSum {lib2,lib3,lib4,lib5} // lib2
//Start = libraryWithHighestSum {lib1,lib3,lib4} // lib4
//Start = libraryWithHighestSum {lib5} //  lib5
//Start = libraryWithHighestSum {} // "No library"  