doubleSmall x = (if x > 100 then x else x/2) + 1
doubleSmall' x = (if x < 100 then x else x/2) + 1	
sumaation x = sum x 
destroy x y = drop x y 
length' xs = sum [ 1 | _ <- xs]
removeUpp st = [c | c <- st, c `elem` ['A'..'Z']]
tup' x y = zip x y
rightAngle = [(a,b,c) | c <- [1..10], b <- [1..c],  a <- [1..b], a^2+b^2==c^2]