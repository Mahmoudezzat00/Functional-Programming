module test
import StdEnv

perfectsquare :: Int -> Bool 
perfectsquare x
| toReal ((x*x) *10) == 10.0 * ((sqrt (toReal x)) * (sqrt (toReal x))) = True
= False
Start = perfectsquare 16
