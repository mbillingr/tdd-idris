
import Data.List
import Data.String
import System.REPL


-- Exercise 1

ex01a : (String, (String, String)) 
ex01a = ("A", "B", "C")

ex01b : List String
ex01b = ["A", "B", "C"]

ex01c : ((String, String), String)
ex01c = (("A", "B"), "C")


-- Exercise 2
palindrome : String -> Bool
palindrome str = str == reverse str

-- Exercise 3
palindromei : String -> Bool
palindromei str = let lstr = toLower str in
                      lstr == reverse lstr

-- Exercise 4
palindrome10 : String -> Bool
palindrome10 str = if length str <= 10 then 
                      False else
                      str == reverse str

-- Exercise 5
palindromeN : Nat -> String -> Bool
palindromeN n str = if length str <= n then 
                      False else
                      str == reverse str

-- Exercise 6
counts : String -> (Nat, Nat)
counts str = let w = length (words str)
                 c = length str in
                 (w, c)

-- Exercise 7
top_ten : Ord a => List a -> List a
top_ten items = take 10 (reverse (sort items))

-- Exercise 8
over_length : Nat -> List String -> Nat
over_length n strs = length (filter (> n) (map length strs))

--Exercise 9
show_palindrome : String -> String
show_palindrome str = str ++ "is" ++ (if palindrome str then " " else " not ") ++ "a palindrome.\n"

main_palindrome : IO ()
main_palindrome = repl "Enter a string: " show_palindrome

show_counts : String -> String
show_counts str = show (counts str) ++ "\n"

main_counts : IO ()
main_counts = repl "Enter a string: " show_counts