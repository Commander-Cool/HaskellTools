--IMPORTS--
import System.Random
import Control.Monad
--------------------

myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (xs) = head (tail (reverse xs))

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty List"
elementAt (x:xs) 0 = x
elementAt (x:xs) y = elementAt xs (y-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x:[]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = if (x == head(reverse xs)) then True && isPalindrome (init xs) else False

data NestedList a = Elem a | List [NestedList a]
					deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: (Eq a)=>[a] -> [a]
compress [] = []
compress (x:xs) = x : compress(dropWhile (==x) xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = ((takeWhile (==x) xs)++[x]):pack (dropWhile (==x) xs)

encode :: (Eq a) => [a]->[(Int, a)]
encode [] = []
encode (x:xs) = (length ((takeWhile (==x) xs)++[x]), x):[]++ encode (dropWhile (==x) xs)

data ListItem a = Single a | Multiple Int a
					deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified [] = []
encodeModified (x:xs) = (if length ((takeWhile (==x) xs)++[x]) == 1 
							then [Single x] 
								else [Multiple (length ((takeWhile (==x) xs)++[x])) x]) ++ encodeModified (dropWhile (==x) xs)

decode :: Eq a => [ListItem a] -> [a]
decode = concatMap decode'
	where 
		decode' (Single y) = [y]
		decode' (Multiple x y) = replicate x y

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

dupli :: [a] -> [a]
dupli = concatMap dupli' 
	where dupli' x = replicate 2 x 

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs y  =  take (y-1) xs ++ dropEvery (drop y xs) y

split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs y z = if y==z
					then [xs !! z]
						else
							[xs !! y] ++ slice xs (y+1) z

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = tail xs ++ [head xs]
rotate xs y = if y<0
				then rotate xs (y+length xs)  
					else(drop y xs) ++ (take y xs )

removeAt :: Int -> [a] -> (a, [a])
removeAt x y = (y !! x, (take (x-1) y) ++ (drop x y))

insertAt :: a -> [a] -> Int -> [a]
insertAt x y z = (take z y) ++ [x] ++ (drop z y)

range :: Int -> Int -> [Int]
range x y = if x==y
				then [x]
					else [x] ++ range (x+1) y

rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

diff_select :: Int -> Int -> IO [Int]
diff_select n to = diff_select' n [1..to]
diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0,(length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)
