import Data.Char (chr)

cons8 list = 8 : list

cons8end list = list ++ [8]

myCons :: [a] -> a -> [a]
myCons list thing = thing : list

headAndTail :: [a] -> (a, [a])
headAndTail list = (head list, tail list)

-- will crash if list has less than 5 elements
fifthElement :: [a] -> a
fifthElement list = head (tail (tail (tail (tail list))))

h :: Int -> a -> b -> Char
h x y z = chr (x - 2)
