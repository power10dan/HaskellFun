data MyTree a = MyEmptyNode 
               | MyFilledNode a (MyTree a) (MyTree a)
               deriving (Eq, Show)

-- insert into binary search (also handles creation)
insertBinaryTree :: (Ord a)=> MyTree a -> a -> MyTree a
insertBinaryTree MyEmptyNode x = MyFilledNode x MyEmptyNode MyEmptyNode 
insertBinaryTree (MyFilledNode a l r) x 
                  | x == a = MyFilledNode a l r 
                  | x > a = MyFilledNode a l (insertBinaryTree r x )
                  | otherwise = MyFilledNode a (insertBinaryTree l x) r 

-- pre-order traversal
preOrderTrav:: MyTree a -> [a]
preOrderTrav MyEmptyNode = [] 
preOrderTrav (MyFilledNode x l r) = [x] ++ (preOrderTrav l) ++ (preOrderTrav r)
-- post-order traversal 
postOrderTrav :: MyTree a -> [a]
postOrderTrav MyEmptyNode = []
postOrderTrav (MyFilledNode x l r) = (postOrderTrav l) ++  (postOrderTrav r) ++ [x]

-- main 
main :: IO ()
main = 
	do 
		let myT = MyFilledNode 2 (MyFilledNode 3 MyEmptyNode MyEmptyNode) (MyFilledNode 4 MyEmptyNode MyEmptyNode)
		print (insertBinaryTree myT 5)
		print (preOrderTrav myT)
		print (postOrderTrav myT)

