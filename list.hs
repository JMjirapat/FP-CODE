len :: Num p => [a] -> p
len [] = 0
len (_:xs) = 1 + len xs

join :: ([a],[a]) -> [a]
join ([],xs) = xs
join (x:xs,ys) = x: join (xs,ys) -- O(n)

zipper :: ([a],[b]) -> [(a,b)]
zipper ([],_) = []
zipper (_,[]) = []
zipper (x:xs,y:ys) = (x,y): zipper(xs,ys)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = join (rev xs,x:[]) -- O(n2)
-- no

rev' :: [a] -> [a]
rev' [] = []
rev' l = rev_aux l []
  where
    rev_aux [] = \xs -> xs
    rev_aux (x:xs) = \ys -> rev_aux xs (x:ys)

-- rev_aux :: [a] -> [a] -> [a]
-- rev_aux [] = \xs -> xs
-- rev_aux (x:xs) = \ys -> rev_aux xs (x:ys) -- O(n)


join' :: [a] -> [a] -> [a]
join' [] ys = ys
join' (x:xs) ys = x : join' xs ys

zipper' :: [a] -> [b] -> [(a,b)]
zipper' = \x y -> zipper(x,y)

fac :: Integral t => t -> t
fac n
  | n == 0    = 1
  | n > 0     = n * fac (n-1)
  | otherwise = error "negative number"

fac' :: Integral t => t -> t
fac' n 
  | n >= 0 = fac_aux n 1
  | otherwise = error "negative number"
  where
    fac_aux 0 res = res
    fac_aux x res = fac_aux (x-1) (x*res)

-- fac_aux :: Integral t => t -> t -> t
-- fac_aux 0 res = res
-- fac_aux n res = fac_aux (n-1) (n*res)

fib :: Integral t => t -> t
fib n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fib (n-1) + fib (n-2)
  | otherwise = error "negative number" -- O(2n)
-- no, i want to cache old computed result

fib' :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib' n = fib_aux 0 0 1
  where
    fib_aux i res res'
      | i == n = res
      | otherwise =
        fib_aux (i+1) res' (res+res')

sum :: Num a => [a] -> a
sum [] = 0
sum xs = sum_aux xs 0
  where
    sum_aux [] res = res
    sum_aux (y:ys) res = sum_aux ys (res+y)

-- sum_aux :: Num a => [a] -> a -> a
-- sum_aux [] res = res
-- sum_aux (x:xs) res = sum_aux xs (res+x)

list_map :: (a -> b) -> [a] -> [b]
list_map _ [] = []
list_map f (x:xs) = f x : (list_map f xs)

list_map' :: (a -> b) -> [a] -> [b]
list_map' _ [] = []
list_map' f l = rev' (list_map_aux l [])
  where
    list_map_aux [] res = res
    list_map_aux (x:xs) res = list_map_aux xs ((f x):res)

zipper'' :: [a] -> [b] -> [(a,b)]
zipper'' [] _ = []
zipper'' _ [] = []
zipper'' xs ys = rev' (zipper_aux xs ys [])
  where
    zipper_aux [] _ res = res
    zipper_aux _ [] res = res
    zipper_aux (x:xs) (y:ys) res = zipper_aux xs ys ((x,y):res)

-- fibs = 0:1:zipWith (+) (tail fibs) fibs
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f l = rev (filter_aux f l [])
  where
    filter_aux _ [] res = res
    filter_aux f (x:xs) res
      | f x = filter_aux f (xs) (x:res)
      | otherwise = filter_aux f (xs) (res)

filter_concat :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat _ [] = []
filter_concat f (x:xs)
  | f x = join' x (filter_concat f xs)
  | otherwise = filter_concat f xs

filter_concat' :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat' _ [] = []
filter_concat' f l = flatten $ filter' f l
  where
    flatten [] = []
    flatten (x:xs) = join' x (flatten xs)

filter_concat'' :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat'' f l = concat $ filter' f l

take_while :: (a -> Bool) -> [a] -> [a]
take_while _ [] = []
take_while pred l = rev (take_while_aux pred l [])
  where
    take_while_aux _ [] res = res
    take_while_aux pred (x:xs) res
      | pred x = take_while_aux pred xs (x:res)
      | otherwise = res

take_while' :: (a -> Bool) -> [a] -> [a]
take_while' _ [] = []
take_while' pred (x:xs)
  | pred x = x:(take_while' pred xs)
  | otherwise = []

-- contains1 = ((any) . (<))
-- contains2 = ((.(<)) . (flip any))

len_comp :: Num a => [a] -> a
len_comp l = Prelude.sum [1 | _ <- l]

-- pairEven :: Num a => [a] -> [a] -> [(a,a)]
-- pairEven [] _ = []
-- pairEven _ [] = []
-- pairEven (x:xs) (y:ys)
--   | even $ x + y = (x,y): (pairEven xs ys)
--   | otherwise = pairEven xs ys
contains1 :: Ord p => p -> [p] -> Bool
contains1 = (any . (<))

contains2 :: Ord p => [p] -> p -> Bool
contains2 = (.(<)) . (flip any)

catesianPair :: Integral p => [(p,p)]
-- catesianPair = [(x,y) | x <- [2,3,5], y <- [1,2,4], Prelude.even $ x+y]
-- catesianPair = concat $ map (\y -> map (\x -> (x,y)) [2,3,5]) [1,2,4]
catesianPair = concat $ map (flip map [1,2,4]) $ map (,) [2,3,5]

filterPairEven :: Integral p => [(p,p)] -> [(p,p)]
filterPairEven [] = []
filterPairEven l = filter' (\(x,y) -> even $ x + y) l

catesianPairEven :: Integral p => [(p,p)]
catesianPairEven = filterPairEven catesianPair

partition :: (p -> Bool) -> [p] -> ([p],[p])
partition _ [] = ([],[])
partition p (x:xs)
  | p x = (x:l,r)
  | otherwise = (l,x:r)
  where (l,r) = partition p xs

filter'' :: (p -> Bool) -> [p] -> [p]
-- filter'' _ [] = []
filter'' = ((.) fst) . partition

qsort :: Ord p => [p] -> [p]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
  where (l,r) = partition (<x) xs

reverse_foldl :: [a] -> [a]
reverse_foldl = foldl ((flip (:))) []

reverse_foldr :: [a] -> [a]
reverse_foldr = foldr (\x acc -> acc ++ [x]) []

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f = foldr ((.) (:) f) []

map_foldl :: (a -> b) -> [a] -> [b]
map_foldl f l = rev' $ foldl (\acc x -> f x : acc) [] l

filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr f = foldr (\x -> if f x then (x:) else id) []

all' :: (a -> Bool) -> [a] -> Bool
-- all' pred l = foldr (\x acc -> if not (pred x) then False else acc) True l
all' pred l = foldr (\x acc -> acc && pred x) True l

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = if e == x then True else elem' e xs
-- elem' e l = elem_aux e l False
--   where 
--     elem_aux _ [] res = res
--     elem_aux e (x:xs) res = res || elem_aux e xs res

elem_fold e l= foldl (\acc x -> acc || e == x) False l

partition' :: (p -> Bool) -> [p] -> ([p],[p])
partition' p l = foldr (\x (lt,rt) -> if p x then (x:lt,rt) else (lt,x:rt)) ([],[]) l
-- partition' _ [] = ([],[])
-- partition' p (x:xs)
--   | p x = (x:l,r)
--   | otherwise = (l,x:r)
--   where (l,r) = partition p xs

partition'' p = foldr (select) ([],[])
  where
    select x (l,r)
      | p x = (x:l,r)
      | otherwise = (l,x:r)

foldl' f acc l = foldr (flip f) acc l

foldr' f acc l = foldl (flip f) acc (rev l) 

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show)

daysInMonth m = case m of
  January -> 31
  February -> 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

nextMonth :: Month -> Month
nextMonth m = case m of
  January -> February
  February -> March
  March -> April
  April -> May
  May -> June
  June -> July
  July -> August
  August -> September
  September -> October
  October -> November
  November -> December
  December -> January

nextDay :: Integer -> Month -> (Integer,Month)
nextDay d m
  | (d `mod` daysInMonth m) == 0 = (1,nextMonth m)
  | otherwise = (d+1,m)