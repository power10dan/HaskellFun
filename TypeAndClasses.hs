removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z 

factorial :: Integer -> Integer
factorial x = product [1..x]

lucky :: (Integral a) => a -> String 
lucky 7 = "Hahah"
lucky x = "you are out of luck pa "

factRecur :: (Integral a) => a -> a
factRecur 0 = 1
factRecur x = x * factRecur(x-1)

addVec :: (Num a ) => (a, a) -> (a,a) -> (a,a)
addVec a b = (fst a + fst b, snd a + snd b)

addVecPattern :: (Num a) => (a,a) -> (a,a) ->(a,a)
addVecPattern (x1,y1) (x2,y2) = (x1+x2, y1+y2)

first :: (a, b, c) -> a
first (x, _, _) = x 

pattMatchList :: [(Integer, Integer)] -> [Integer]
pattMatchList xs = [a+b | (a,b) <- xs]

head' :: [a] -> (a,a)
head' [] = error "Not good"
head' (x:y:_) = (x,y)

tell:: (Show a ) => [a] -> String 
tell [] = "List is empty"
tell (x:[]) = "The list only has one element:  " ++ show x 
tell (x:y:[]) = "Two elements:  " ++ show x ++ show y 
tell (x:y:_) = "List is long:   " ++ show x ++ show y 

length' :: (Num b)=> [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

capital :: String -> String 
capital "" = "Empty"
capital all@(x:xs) = "First letter" ++ all ++ [x]




