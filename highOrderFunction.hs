-- curried function example
multtwo :: (Num a) => a -> a-> a
multtwo x y = x*y 

multthree :: (Num a) => a -> a -> a -> a
multthree x y z = (multtwo x y)* z

-- partially applied function 
comparedWithHundred :: (Num a, Ord a) => a -> Ordering
comparedWithHundred = compare 100 

divideByTen :: (Floating a) => a -> a
divideByTen  = (/10 )

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _  = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 