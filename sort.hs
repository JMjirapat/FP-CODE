quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (fst part) ++ [x] ++ quickSort (snd part)
  where part = partition (<= x) xs
        partition pred = foldr (\x acc -> if pred x then (x:(fst acc), snd acc) else (fst acc, x:(snd acc))) ([], [])

merge :: (Ord a) => [a] -> [a] -> [a]
merge l [] = l
merge [] r = r
merge (l:ls) (r:rs) = if l > r then r:(merge (l:ls) rs) else l:(merge ls (r:rs))
      
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (x:xs) = merge (mergeSort left) (mergeSort right)
  where (left, right) = splitAt (length xs `div` 2) (x:xs)
        splitAt n l = foldr (\x acc -> if (length $ fst acc) <= n then (x:(fst acc),snd acc) else (fst acc, x:(snd acc))) ([],[]) l

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where insert x [] = [x]
        insert x (y:ys) = if x < y then x:y:ys else y:(insert x ys)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort l = bubbleSort' l (length l)
  where bubbleSort' l 0 = l
        bubbleSort' l n = bubbleSort' (bubble l) (n-1)
        bubble [x] = [x]
        bubble (x:y:xs) = if x > y then y:(bubble (x:xs)) else x:(bubble (y:xs))

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort l = selectionSort' l []
  where selectionSort' [] acc = acc
        selectionSort' l acc = [(minList l)] ++ (selectionSort' (remove (minList l) l) acc)
        remove x (y:ys) = if x == y then ys else y:(remove x ys)
        minList [x] = x
        minList (x:xs) = if x < minList xs then x else minList xs
          