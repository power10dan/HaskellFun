import Data.Function (on)
import Data.List
import Data.Ord

topKFrequent :: [Int] ->  Int -> [Int]
topKFrequent x a =  take a $ reverse $ fst <$> sortList  sortedGroupTupleList
                     where sortedGroupTupleList = (map (\x -> (head x, length x)) . group . sort $ x)
                           sortList = sortBy (comparing snd)

kthLargest :: [Int] -> Int -> Int
kthLargest listOfTup a = (reverse . sort . reverse ) listOfTup !! (a -1)

majorityElem :: [Int] -> Int -> [(Int, Int)] 
majorityElem [] _ = [(0,0)]
majorityElem list lenL = filter ((\p -> p > (div lenL 2)).snd ) mapSortedTuple
                        where mapSortedTuple = (map (\x-> (head x, length x)) . group . sort $ list)
                                    
reverseVowel :: String -> String 
reverseVowel inString = 