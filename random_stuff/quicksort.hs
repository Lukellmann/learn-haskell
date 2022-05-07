quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot : tail) = join smaller_equal_sorted pivot bigger_sorted
  where
    (smaller_equal, bigger) = split pivot tail
    smaller_equal_sorted = quicksort smaller_equal
    bigger_sorted = quicksort bigger

split :: Ord a => a -> [a] -> ([a], [a])
split _ [] = ([], [])
split pivot (x : xs) =
  let (smaller_equal, bigger) = split pivot xs
   in if x <= pivot then (x : smaller_equal, bigger) else (smaller_equal, x : bigger)

join :: [a] -> a -> [a] -> [a]
join [] b c = b : c
join (a_head : a_tail) b c = a_head : joined_tail
  where
    joined_tail = join a_tail b c
