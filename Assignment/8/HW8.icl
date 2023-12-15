module HW8 
import StdEnv

//Please write your name and Neptun code
/* From the given 3 problems choose any 2 to get the full score */

//Trees for testing please do not remove or modify
:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))

tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))

tree5 :: Tree Int
tree5 = Node 1 tree3 tree4
/*
	Given a key determine in which level it is stored in the Tree.
	
	
		 07           <- Level 0
	   /   \          
	 02	    20        <- Level 1
	 /\	    / \ 
	01 04  10 30 	  <- Level 2 
	If the value is not in the tree then return -1
*/			
max :: Int Int -> Int 
max n1 n2 
| n1 > n2 = n1
| otherwise = n2 

treeToList :: (Tree a) -> [a]
treeToList (Leaf) = []
treeToList (Node x left right) = [x] ++ treeToList left ++ treeToList right

//Start = treeToList tree2

getLevel :: Int (Tree Int) -> Int
//getLevel num (Node x le ri) = hd [x \\ y <- list  & x <- [0,1..] || y == num ]
//getLevel num (Node x left right) = hd [lvl \\ (val, lvl) <- (zip (treeToList(Node x left right)) [0..]) || val == num]

getLevel num tree = go 0 tree
  where go level Leaf = -1 
        go level (Node x left right)
          | x == num  = level
          | otherwise = max (go (level + 1) left) (go (level + 1) right)
//where list = treeToList(Node x le ri)


//Start = getLevel 5 tree2 // 0
//Start = getLevel 10 tree1 // 1
//Start = getLevel 55 tree1 // -1
//Start = getLevel 31 tree2 // 2






/* Given a binary tree, find how many nodes are there such that they have exactly
3 grandchildren non-leaf nodes.
Ex.:  1
    /   \
    2    3
   / \  / \
  4 5   6 Leaf
1st node has exactly 3 grandchildrens, so it's a 'good' node.
*/
/*
h :: (Tree Int) (Tree Int)-> Bool 
h (Leaf) a = False
h a (Leaf) = False 
h (Node x left right) (Node a le ri)
|left <> Leaf && right <> Leaf && le <> Leaf && ri <> Leaf = (count left + count right + count le + count ri) == 3  
= False
count :: (Tree Int) -> Int 
count  (Node x left right) 
| x <> Leaf = 1 
= 0

countTripleParents :: (Tree Int) -> Int
countTripleParents (Node x le ri) 
|(h le ri) = 1 + countTripleParents le + countTripleParents ri
= 0
*/
//Start = countTripleParents tree1 // 1
//Start = countTripleParents tree2 // 1
//Start = countTripleParents tree3 // 1
//Start = countTripleParents tree4 // 4
//Start = countTripleParents tree5 // 5

//do not remove these
:: City = BUDAPEST | GYOR | DEBRECEN  
:: Product = {productName :: String , price :: Real} 
:: Shop = {shopName :: String , products :: {Product}, location :: City}

meat       ={productName ="meat"   ,price= 5000.123 }
fruits     ={productName ="fruits" , price=2000.123 }
vegetables ={productName ="vegetables",price=1700.50}

aldi = {shopName = "aldi" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
spar = {shopName = "spar" , products = {{productName ="meat"   ,price= 4500.0},fruits,vegetables} , location = GYOR}
lidl = {shopName = "lidl" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
abc  = {shopName = "abc" , products =  {{productName ="meat"   ,price=  4500.0},{productName ="fruits"   ,price=  1700.0},{productName ="vegetables"   ,price=  1500.0}} , location = GYOR}


/* 2 - Shops 
    Given an array of shops, all the shops have the same products but with different prices,
    return a tuple containing the name of the cheapest shop and its location. The cheapest shop is
    the shop in the array whose sum of its products prices is the smallest in the array.
    
    Note : if there is more than one cheap shop in the array, return the first one.
    Assume that the given array is not empty. 
*/

sumOfProductsPerShop :: Shop -> Real
sumOfProductsPerShop sh = sum [x.price\\ x<-: sh.products]
//Start = sumOfProductsPerShop abc

cheapestShop :: {Shop} -> (String,City)
cheapestShop arr = hd[(x.shopName, x.location)\\ x<-: arr | cheapesttotal == sumOfProductsPerShop x ]
where
 cheapesttotal = hd (sort [sumOfProductsPerShop x \\ x<-: arr ])
 

//Start = cheapestShop {aldi,spar,lidl,abc} // ("abc",GYOR)
//Start = cheapestShop  {aldi,spar,lidl,abc} // ("abc",GYOR)
//Start = cheapestShop  {aldi,spar} // ("spar",GYOR)
//Start = cheapestShop  {lidl,aldi} // ("lidl",BUDAPEST)
