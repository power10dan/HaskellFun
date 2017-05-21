-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smallsort = quicksort [a | a<-xs, a <= x]
	    bigsort = quicksort[a | a<-xs, a > x]
	 in smallsort ++ [x] ++ bigsort
	 
-- merge sort 
merge:: (Ord a)=> [a]->[a]->[a]
merge xs [] = xs 
merge [] ys = ys 
merge (x:xs) (y:ys) 
    | (x <= y) = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

splithalf' :: (Ord a) => [a] -> ([a], [a])
splithalf' x  = (take half x, drop half x)
    where half = (length x) `div` 2 

mergesort' :: (Ord a) => [a] -> [a]
mergesort' list 
     | (length list) > 1 = merge (mergesort' ls) (mergesort' rs)
     | otherwise = list 
     where (ls, rs) = splithalf' list 





