bmiTell :: (RealFloat a) => a -> String
bmiTell bmi 
	| bmi < 19.2 = "hahahaha"
	| bmi > 25.3 = "booooo"
	| otherwise = "booo"

max' :: (Ord a) => a -> a ->a
max' a b 
	| a > b = a
	| otherwise = b 

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
  | bmi < weight = "haha"
  | bmi > height = "booo"
  where bmi = weight / height


lastName :: String -> String ->String 
lastName first last = [f]
    where (f:_) = first

calcB ::  (Fractional a) => [(a, a)] -> [a]
calcB xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height


lengths' :: [a] -> Int
lengths' [] =  0
lengths' (x:xs) = 1 + lengths' xs 


