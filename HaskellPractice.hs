{-Andrew Gerlach
 A few Haskell functions 
-}

import Data.List
{-
--Function fn1a
--Input: [Int]
--a list of integers
--Returns a list consisting of the integers which are both positive and even
fn1a :: [Int] -> [Int]
fn1a [] = []
fn1a x = filter (checkNeg) (map modEdit x)

modEdit :: Int -> Bool
modEdit a = (mod a 2 == 0)
-}



--Function fn1b: doubles
--Input: [Int]
--a list of integers
--Returns a list cinsisting of the doubles of the positive values in the input.
fn1b :: [Int] -> [Int]
fn1b [] = []
--only want positive numbers
fn1b x = filter (checkNeg) (map doubleHelper x)

checkNeg :: Int -> Bool
--true if x > 0
checkNeg x = (x > 0)

doubleHelper :: Int -> Int
doubleHelper x
  | x >= 0 = x * 2
--returning a negative value just to help remove
  | otherwise = -1



--Function fn1c: square root
--Input: [Int] Int
--an integer and a guess for what the square root of the integer is
--outputs an INFINITE list of the approximations of the square root
--Stop output using ctrl-c.
fn1c :: Fractional a => a -> a -> [a]
fn1c x y = y : fn1c x ((y + (x/y))/2)

--Function fn1d: 
--Input: [Int] Int
--a list of integers and an integer x
--The function finds the first pair of consecutive integers
--that differ by no more than x
fn1d :: [Int] -> Int -> Int
fn1d (x:xs) y = fn1dHelp x xs y

fn1dHelp :: Int -> [Int] -> Int -> Int
fn1dHelp a (b:bs) y
  | fn1dHelp1 a b y = b
  | otherwise = fn1d (b:bs) y

fn1dHelp1 :: Int -> Int -> Int -> Bool
fn1dHelp1 x y z = (abs (x-y)) <= z

--Function fn1e:
--Input: x t
--x and t are numbers
--Return the first estimate to the square root of the x that is within t
--of the previous estimate.
--The square roots are generated as in Function fn1c
fn1e :: Double -> Double -> Double
fn1e x z = fn1eHelp (x/2) (((x/2) + (x/(x/2)))/2) z

fn1eHelp :: Double -> Double -> Double -> Double
fn1eHelp x y z
  | abs (x-y) <= z = y
  | otherwise = fn1eHelp y ((y + (x/y))/2) z

--Function fn1f: List of functions and elements
--Input: [b->a] [b]
--A list of functions and a list of the functions' appropriate inputs
--The first function is applied to the first element of the second list
--The second function applies to the second element of the second list etc...
fn1f :: [b->a] -> [b] -> [a]
fn1f [] [] = []
fn1f [] y = []
fn1f x [] = []
fn1f (x:xs) (y:ys) = x y : fn1f xs ys

--Function fn1g: Multiple functions, one value
--Input: [b->a] b
--Outputs a list containing the results of the list of functions on b.
fn1g :: [b->a] -> b -> [a]
fn1g [] y = []
fn1g (x:xs) y = x y : fn1g xs y

--Function fn1h: remove occurences of first argument
--Input a [a]
--'a' is something that is comparable
--[a] is a list containing the same type as the first argument
--Returns a list will all occurrences of the first
--argument removed from the list.
fn1h :: Ord a => a -> [a] -> [a]
fn1h x [] = []
fn1h x y = filter (/= x) y

--Function fn1i: remove occurences from list
--Input [a] [a]
--Two lists of the same type
--Removes all of the occurrences of the elements in the first list
--from the second list and returns it.
fn1i :: Ord a => [a] -> [a] -> [a]
fn1i [] y = y
fn1i (x:xs) y = fn1iHelp x xs y

--Recursively call 'fn1i' 
--with cdr and previous list with first element (car) removed
fn1iHelp :: Ord a => a -> [a] -> [a] -> [a]
fn1iHelp x z y = fn1i z (fn1h x y)

--Function fn1j: Build prefixes
--Input: [a]
--Returns a list of the prefixes of the input
--Example: input: [0,1,2,3] -> [[],[0],[0,1],[0,1,2],[0,1,2,3]]
fn1j :: [a] -> [[a]]
-- Use foldr to build the list of list of prefixes 
fn1j x = [] : foldr (:) [] (fn1jHelp [] x)

-- Recursively call this method to build the unique prefixes
fn1jHelp :: [a] -> [a] -> [[a]]
fn1jHelp y [] = []
--build it up using the first element
--then call the function again using what was previously made and the rest of the list
fn1jHelp y (x:xs) = (y ++ [x]) : (fn1jHelp (y ++ [x]) xs)

--Function fn1k powerset
--Input: [a]
--Returns the powerset of the input list
--a powerset is found by;
--removing the first element off of the set
--mapping that element to the power set of one less element
--adding the powerset without that element added
fn1k :: [a] -> [[a]]
fn1k [] = [[]]
fn1k (x:xs) = map (x:) (fn1k xs) ++ (fn1k xs)






-- q2 insertion sort
--Input: [Int]
--Returns the sorted list of Integers using insertion sort
q2 :: [Int] -> [Int]
q2 [] = []
q2 x = foldr insertion [] x
{-
-- insert into the list
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x:(y:ys)
  | otherwise = y : insert x ys
-}

--Question 3
--Input: [String]
--The strings in the list represent the pages in a book.
--Returns the index of the ppok i.e. words and the pages they appear on.
--inverted index
--returns each word and which page it appears on
--call function by entering: invert [*your list of strings*]
invert :: [String]->[(String, Int)]
invert x = (removeDuplicates(putInOrder(makeTuples(putTogether(bookZip x)))))


--zip with a list of integers starting at 1 (page 1)
--this will put each list of words with the page number they're found on
--they are not yet tupes at this point***
bookZip :: [String] -> [([String],Int)]
bookZip x = zip (bookSort x) [1..(length x)]

--Sorts the books 'pages' into alphabetized lists of words
bookSort :: [String] -> [[String]]
bookSort (x:xs) = (insertionSort (words x)):(bookSort xs)
bookSort [] = []

--same insertion sort as question 2 just in a more generalized definition
insertionSort ::Ord a => [a] -> [a]
insertionSort [] = []
insertionSort x = foldr insertion [] x

insertion :: Ord a => a -> [a] -> [a]
insertion x [] = [x]
insertion x (y:ys)
 | x <= y = x:(y:ys)
 | otherwise = y : insert x ys

--takes the list of lists of page/page number pairs and puts them into one list
putTogether :: [([String],Int)] -> [([String],Int)]
putTogether (x:xs) = x:(putTogether xs)
putTogether [] = []

--this puts each word into a tuple with its corresponding page number
--will look like: [...[(["word1","word2",..."wordn"],1)]...}
makeTuples :: [([String],Int)] -> [(String,Int)]
makeTuples (x:xs) = (makeTuplesHelp [x])++(makeTuples xs)
makeTuples [] = []

makeTuplesHelp :: [([String],Int)] -> [(String,Int)]
--you've run out of words, return an empty list
makeTuplesHelp [([],n)] = []
--take 1 word and put it in a tuple with th epage number
--then call the method again using the rest of the list with the same number
makeTuplesHelp [((x:xs),n)] = (x,n) : (makeTuplesHelp [(xs,n)])


--insertion sort for (String,Int) tuples
--ordered by the word in the first part of the tuple
putInOrder :: [(String,Int)] -> [(String,Int)]
putInOrder x = foldr insert2 [] x

insert2 :: (String, Int) -> [(String,Int)] -> [(String,Int)]
insert2 x [] = [x]
insert2 x (y:ys)
--look at the string and compare
--if it's 'lower' put it in front
 | (fst x) <= (fst y) = x:(y:ys)
 | otherwise = y : insert2 x ys

--remove any words that appear more than once on a given page
--filter out everything that is the same as x
--meaning same word and integer
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
removeDuplicates [] = []




-- q4: Prime Number list
--Input: 2
--called by entering: q4 2 
--Returns infinite list of primes
--Run must be stopped using ctrl-c
q4 :: Int -> [Int]
q4 x = x : q4Help [x] (x+1)

q4Help :: [Int] -> Int -> [Int]
q4Help x y
--if this operation results in an empty list, then this number is prime.
--add it to the the list, continue with the next integer
--use method 'take' when using this function to not get an infinite list result
  | filter (==0) (map (mod y) x) == [] = y : q4Help (x++[y]) (y+1)
  | otherwise = q4Help x (y + 1)


--Q5 alternate Sieve using Haskell library
--this function just gets the number for the end of the list
--uses the module Data.List and uses its (\\) operator for a different
--method to make a list of primes.
--Input: Int
--Returns a list of all of the primes less than or equal to the input Integer
sieve :: Int -> [Int]
--create a list from 2 to n and send it to 'sieve'
sieve n = sieveHelp n [2..n]

{-recursive function which will take the first number in the list
which will always be prime and remove all multiples of that number 
starting with the square of that number. Any multiple less than that square
will already have been removed by that time.-}
sieveHelp :: Int -> [Int] -> [Int]
{-(\\) is the set difference function. Here we remove all multiples of 
the current prime we are considerng starting with the square of that prime-}
sieveHelp n (x:xs) = x : sieveHelp n ((\\) xs [x * x, x * x + x .. n])
sieveHelp n [] = []