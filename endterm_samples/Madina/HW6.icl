module HW6
import StdEnv


:: University = Elte | Corvinus | BME

:: UniRelation = Teacher | Student

:: Course = FP | OOP | DB | AI | ML


/*
Create a record Citizen with the following fields:
id::Int
uni::University
grades::[Int]
courses::[Course]
rel::UniRelation
*/

:: Citizen={id::Int
			,uni::University
			,grades::[Int]
			,courses::[Course]
			,rel::UniRelation}

/*
You are given an array of citizens , write a function that returns the list of (id,uni,rel) pair of citizens 
who will be awarded. a Citizen can be awarded if he/she has at least 3 courses , the average of his/her grades is at least 3.

*/

AwardCitizen :: {Citizen} -> [(Int,University,UniRelation)]
AwardCitizen a = [(x.id, x.uni, x.rel) \\ x <-: a | ((avg x.grades) >= 3) && ((length x.courses)>=3)]



// Start =  AwardCitizen {{id = 1,uni = Elte, grades = [4,2,4,1,4,6], courses = [FP,DB,AI] ,rel = Student } , {id = 2,uni = Elte, grades = [4,6,5,2,4,1,4,6], courses = [FP,ML] ,rel = Student }, {id = 3,uni = BME, grades = [4,2,5,1,8,10,4,6], courses = [FP] ,rel = Teacher }}
// [(1,Elte,Student)]

// Start =  AwardCitizen {{id = 1,uni = Elte, grades = [4,2,4,1,4,6], courses = [FP,DB,AI] ,rel = Student } , {id = 2,uni = Elte, grades = [4,6,5,2,4,1,4,6], courses = [FP,DB,AI,ML] ,rel = Student }, {id = 3,uni = BME, grades = [4,2,5,1,8,10,4,6], courses = [FP,DB,AI] ,rel = Teacher }}
// [(1,Elte,Student),(2,Elte,Student),(3,BME,Teacher)]

// Start  =  AwardCitizen {} // []


/*
Write a function that takes an array of Strings and removes consonants from each string.

Hint : a string is an array of characters
*/

RemoveCons :: {String} -> {String}
RemoveCons a = {{y \\ y<-:x | (isMember y ['a','e','i','o','u','A','E','I','O','U']) } \\ x <-: a}

// Start = RemoveCons {"hello","world","how","are","you"} // {"eo","o","o","ae","ou"}
// Start = RemoveCons {"Functional","Programming","is","fun"} // {"uioa","oai","i","u"}

