-- Problem 1 
myLast :: [a] -> a
myLast x = head (drop ((length x) -1) x)

myLast' :: [a]-> a
myLast' x = head (reverse x)

-- Problem 2
secondLast :: [a] -> [a]
secondLast x = drop 1 (take 2 (reverse x))

-- Problem 3
elemWith' :: [a] -> Int -> a
elemWith' list 0 = error "bad number"
elemWith' list a 
       | a > length list = head (reverse(list)) 
       | otherwise  = head (reverse (take a list))

-- Problem 4
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Problem 5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (list == reverse list)

