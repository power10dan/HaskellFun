maxawesome :: (Ord a) => [a] -> a
maxawesome [] = error "Bad list"
maxawesome [x] = x
maxawesome (x:xs)   
    | x > maxTail = x   
    | otherwise = maxTail  
    where maxTail = maxawesome xs

maxawesome' :: (Ord a) => [a] -> a
maxawesome' [] = error "Empty"
maxawesome' [x] = x
maxawesome' (x:xs ) = max x (maxawesome' xs)

replicate' :: (Num i, Ord i) => i->a ->[a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate'(n-1) x 

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
   | n <= 0 = []
   | otherwise = x : take' (n-1) xs 

reverse' :: (Ord a)=> [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip':: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool 
elem' _ [] = False 
elem' n (x:xs) 
    | n == x  = True
    | otherwise = elem' n xs 


