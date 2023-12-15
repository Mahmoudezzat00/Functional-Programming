module HW7
import StdEnv

/* 50p
You are given a string s representing an attendance record for a student where each character signifies 
whether the student was absent, late, or present on that day. The record only contains the following three characters:

'A': Absent.
'L': Late.
'P': Present.
The student is eligible for an attendance award if they meet both of the following criteria:

The student was absent ('A') for strictly fewer than 2 days total.
The student was never late ('L') for 3 or more consecutive days.
Return true if the student is eligible for an attendance award, or false otherwise. */



//f1 :: String -> Boolean

//Start = f1 "PPALLP" //true
//Start = f1 "PPALLL" //false



/* (25p) Given the Skill algebraic type of 3 skill levels 
make a Competitor record that contains the name, skill level, and the number of coding languages(clang) he/she can use.
There is a rule that a competitor cannot be Beginner level and should know at least 3 coding languages. 
Given a competitor decide whether they qualify for the competition */

//:: Skill = Beginner | Intermediate | Advanced
//:: Competitor = {name :: String, skillLevel :: Skill, clang :: Int}
/*
c1 = {name="John", skillLevel=Beginner, clang=1}
c2 = {name="Bob", skillLevel=Intermediate, clang=3}
c3 = {name="Amy", skillLevel=Beginner, clang=3}

doesQualify :: Competitor -> Bool
doesQualify {skillLevel=Beginner, clang=n} = False
doesQualify {skillLevel=_, clang=n} = n >= 3
*/

//Start = doesQualify c1 // False
//Start = doesQualify c2 // True
//Start = doesQualify c3 // False

/* (25p)

Each array of competitors are a team trying to enter a coding competition.
However, there is a rule that all the members of the team cannot be Beginner level and
there should be at least one Advanced competitor with 3 coding languages in one team. 
Decide if the given teams qualify for the competition. */
:: Skill = Beginner | Intermediate | Advanced
:: Competitor = {name :: String, skillLevel :: Skill, clang :: Int}

team1 = [{name="John", skillLevel=Beginner, clang=1}, {name="Bob", skillLevel=Beginner, clang=2}]
team2 = [{name="John", skillLevel=Advanced, clang=1}, {name="Bob", skillLevel=Intermediate, clang=2}]
team3 = [{name="John", skillLevel=Beginner, clang=1}, {name="Bob", skillLevel=Beginner, clang=2}, {name="Maven", skillLevel=Advanced, clang=4}]

doesQualifyTeam :: [Competitor] -> Bool
doesQualifyTeam [] = False
doesQualifyTeam team = length advancedComp == 1 && length team == length nonBeginnerComps
  where
    advancedComp = filter doesQualify22 team
    nonBeginnerComps = filter doesQualify33 team

doesQualify22 :: Competitor -> Bool
doesQualify22 {skillLevel=Beginner, clang=n} = False
doesQualify22 {skillLevel=Advanced, clang=n} = n >= 3 

doesQualify33 :: Competitor -> Bool
doesQualify33 {skillLevel = Beginner} = False

//Start = doesQualifyTeam team1 // False
//Start = doesQualifyTeam team2 // False
//Start = doesQualifyTeam team3 // True
//Start = doesQualifyTeam [] // False
