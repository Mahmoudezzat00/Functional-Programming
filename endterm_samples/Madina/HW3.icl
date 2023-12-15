module HW3
import StdEnv

//Please Rename the file as YourNameHW3.icl

/*

    You're safe online if all your passwords are strong.
    A password is strong if it is at least 8 characters long and contains at 
    least one uppercase letter, one lowercase letter, one digit and 
    one special character. 
    The special characters are: !@#$%^&*()_+-=[]{}|;':",./<>?~`

    Write a function strongPassword that takes a list of passwords and checks if 
    all your passwords are strong.

    Eg : Input : ["Hello@World9", "HelloWorld1!", "HelloWorld1!"]
        Output : "All passwords are strong"

        Input : ["JohnDow","Hellom","yotoo@123"]
        Output : "All passwords are not strong"

        Hint : use the function fromString x to convert string x to a list of char.

        **** Please dont put your own passwords as a test case ****
*/

specialChars = ['!','@','#','$','%','^','&','*','(',')','_','+','-','=','[',']','{','}','|',';','\'',':',',','.','/','<','>','?','~','`']

isSpecialChar :: Char -> Bool
isSpecialChar x = isMember x specialChars

checkPassword :: String -> Bool
checkPassword "" = False
checkPassword s = (length x >= 8) && ( length(filter isUpper x) <>0 ) && ( length(filter isLower x) <>0 ) && ( length(filter isDigit x) <>0 ) && ( length(filter isSpecialChar x) <>0 )
    where x = fromString s

strongPassword :: [String] -> String
strongPassword [] = " Empty List "
strongPassword list 
| and(map checkPassword list) = "All passwords are strong"
= "All passwords are not strong"


//Start = strongPassword ["Hello@World9", "HelloWorld1!", "Helloworld@123"] // "All passwords are strong"
//Start = strongPassword ["JohnDow","Hellom","yotoo@123"] // "All passwords are not strong"


/*
    Write a function that takes a list of  numbers and returns a list of lists 
    where each sublist contains two numbers where first number is the element from the list 
    and the second number is the percentage of frequency of that element in the list.

    eg : Input : [1,2,1,3] 
        Output : [[1,50],[2,25],[3,25]]
        explanation : 1 -> occurs 2 times in the list , total elements in the list = 4 so its percentage is 50
                    2 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25
                    3 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25

        percentage as integer is fine
*/

FreqPercentagePair :: [Int] Int -> [Int]
FreqPercentagePair list x = [x, (length(filter (\y = (y==x)) list) * 100) / length list]


frequency :: [Int] -> [[Int]]
frequency list = removeDup (map (FreqPercentagePair list ) list)







//Start = frequency [1,2,1,3] // [[1,50],[2,25],[3,25]]
//Start = frequency [1,1,1,3,2,2] // [[1,50],[2,33],[3,16]]
//Start = frequency [0,0,0] // [[1,37],[2,12],[5,25],[0,25]]