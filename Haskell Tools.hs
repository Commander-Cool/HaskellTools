--IMPORTS--
import System.Random
import Control.Monad
--------------------

-- Gets Last element of a list
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Get second last element of a list
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (xs) = head (tail (reverse xs))

-- Get element given an index from a list
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty List"
elementAt (x:xs) 0 = x
elementAt (x:xs) y = elementAt xs (y-1)

-- Get length of list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x:[]

-- Check if a string or array is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = if (x == head(reverse xs)) then True && isPalindrome (init xs) else False

-- Flatten a nested list into a a single list
data NestedList a = Elem a | List [NestedList a]
					deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Remove all consecutive repeating elements in a string or array
compress :: (Eq a)=>[a] -> [a]
compress [] = []
compress (x:xs) = x : compress(dropWhile (==x) xs)

-- return a list of tuples of all like elements together in one tuple
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = ((takeWhile (==x) xs)++[x]):pack (dropWhile (==x) xs)

-- Returns a list of tuples with each of element and the number of times they occur in the list
-- OUTPUT: [(4 A) B (2 C) (2 A) D (4 E)]
encode :: (Eq a) => [a]->[(Int, a)]
encode [] = []
encode (x:xs) = (length ((takeWhile (==x) xs)++[x]), x):[]++ encode (dropWhile (==x) xs)

-- Returns output with each element and whether or not it is Multiple of Single in the list or string
data ListItem a = Single a | Multiple Int a
					deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified [] = []
encodeModified (x:xs) = (if length ((takeWhile (==x) xs)++[x]) == 1 
							then [Single x] 
								else [Multiple (length ((takeWhile (==x) xs)++[x])) x]) ++ encodeModified (dropWhile (==x) xs)

-- Decode a list of type ListItem to a regular list
decode :: Eq a => [ListItem a] -> [a]
decode = concatMap decode'
	where 
		decode' (Single y) = [y]
		decode' (Multiple x y) = replicate x y

-- Encode list into a ListItem type
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys
 
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

-- Double each element in a list
dupli :: [a] -> [a]
dupli = concatMap dupli' 
	where dupli' x = replicate 2 x 

-- Given a list replicate each element n times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Drop every nth element of a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs y  =  take (y-1) xs ++ dropEvery (drop y xs) y

-- Given an index split the list at that point
split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

-- Given two indices get every element within those two indices 
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs y z = if y==z
					then [xs !! z]
						else
							[xs !! y] ++ slice xs (y+1) z
-- Given a pivot element move eberything to the left of the element to the end
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = tail xs ++ [head xs]
rotate xs y = if y<0
				then rotate xs (y+length xs)  
					else(drop y xs) ++ (take y xs )

removeAt :: Int -> [a] -> (a, [a])
removeAt x y = (y !! x, (take (x-1) y) ++ (drop x y))

-- Insert element in a specific position of a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x y z = (take z y) ++ [x] ++ (drop z y)

-- Make list with range of numbers from x to y
range :: Int -> Int -> [Int]
range x y = if x==y
				then [x]
					else [x] ++ range (x+1) y

-- Randomly select an element from a list
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

-- Choose x random elements from list
diff_select :: Int -> Int -> IO [Int]
diff_select n to = diff_select' n [1..to]
diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0,(length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)
