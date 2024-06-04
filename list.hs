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

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show, Eq, Enum)

daysInMonth :: [Integer]
daysInMonth = [31,28,31,30,31,30,31,31,30,31,30,31]

nextMonth :: [Month]
nextMonth = [February .. December] ++ [January]

ansForMonth :: Month -> [a] -> a
-- ansForMonth m ans = snd . head . filter' (\x -> (fst x) == m) . zipWith (,) [January .. December] ans
ansForMonth m = snd . head . filter' (((== m) . fst)) . zip [January .. December]

nextDay :: Integer -> Month -> (Integer,Month)
nextDay d m
  | d == ansForMonth m daysInMonth = (1,ansForMonth m nextMonth)
  | otherwise = (d+1,m)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node l v r) = [v] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node l v r) = postorder l ++ postorder r ++ [v]

map_tree :: (a -> b) -> Tree a -> Tree b
map_tree _ Empty = Empty
map_tree f (Node l v r) = Node (map_tree f l) (f v) (map_tree f r)

fold_tree_preorder :: (a -> b -> b) -> b -> Tree a -> b
fold_tree_preorder _ res Empty = res
fold_tree_preorder f res t = foldr f res $ preorder t

fold_tree_inorder :: (a -> b -> b) -> b -> Tree a -> b
fold_tree_inorder _ res Empty = res
fold_tree_inorder f res t = foldr f res $ inorder t

fold_tree_postorder :: (a -> b -> b) -> b -> Tree a -> b
fold_tree_postorder _ res Empty = res
fold_tree_postorder f res t = foldr f res $ postorder t

treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
treeFold _ res Empty = res
treeFold f res (Node l v r) =
  let acc = treeFold f res l
      acc' = treeFold f acc r
  in f v acc acc'

preorder' :: Tree a -> [a]
preorder' t = treeFold (\v acc acc' -> [v] ++ acc ++ acc') [] t

inorder' :: Tree a -> [a]
inorder' t = treeFold (\v acc acc' -> acc ++ [v] ++ acc') [] t

postorder' :: Tree a -> [a]
postorder' t = treeFold (\v acc acc' -> acc ++ acc' ++ [v]) [] t

height_tree :: Tree a -> Integer
height_tree Empty = 0
height_tree (Node l _ r) = 1 + max (height_tree l) (height_tree r)

data NAryTree a = NAryEmpty | NAryNode a [NAryTree a]

nary_tree_preorder :: NAryTree a -> [a] 
nary_tree_preorder NAryEmpty = []
-- nary_tree_preorder (NAryNode v []) = [v]
nary_tree_preorder (NAryNode v xs) = [v] ++ (concat $ map nary_tree_preorder xs)

nary_tree_postorder :: NAryTree a -> [a] 
nary_tree_postorder NAryEmpty = []
-- nary_tree_postorder (NAryNode v []) = [v]
nary_tree_postorder (NAryNode v xs) = (concat $ map nary_tree_postorder xs) ++ [v]

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node Empty _ Empty) = True
isBST (Node Empty v (Node rl rv rr)) = v <= rv && isBST (Node rl rv rr)
isBST (Node (Node ll lv lr) v Empty) = v > lv && isBST (Node ll lv lr)
isBST (Node (Node ll lv lr) v (Node rl rv rr)) = v <= rv && v > lv && isBST (Node rl rv rr) && isBST (Node ll lv lr)

class IfValue a where
  boolVal :: a -> Bool

instance IfValue Int where
  boolVal 0 = False
  boolVal _ = True

instance IfValue [a] where
  boolVal [] = False
  boolVal _ = True

instance IfValue (Tree a) where
  boolVal Empty = False
  boolVal _ = True

instance (IfValue a, IfValue b) => IfValue (a,b) where
  boolVal (a,b) = boolVal a && boolVal b

mapMaybe :: (a->b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just v) = Just $ f v

mapPair_fst :: (a->b) -> (a,c) -> (b,c)
mapPair_fst f (a,b) = (f a, b)

mapPair_snd :: (c->b) -> (a,c) -> (a,b)
mapPair_snd f (a,b) = (a,f b)

data COp a = CVal Int a deriving (Show)

instance Functor COp where
  fmap f (CVal i a) = CVal (i+1) (f a)

maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing
maybeAp _ Nothing = Nothing
maybeAp (Just f) (Just v) = Just $ f v

initMaybe :: a -> Maybe a
initMaybe = Just

listAp :: [a -> b] -> [a] -> [b]
listAp [] _ = []
listAp _ [] = []
listAp fs vs = [f v | f <- fs, v <- vs]

initList :: a -> [a]
initList = (:[])

newtype BoolAnd = BoolAnd { getBoolAnd :: Bool }
  deriving (Show)
instance Semigroup (BoolAnd) where
  (BoolAnd a) <> (BoolAnd b) = BoolAnd (a && b)
instance Monoid (BoolAnd) where
  mempty = BoolAnd True

newtype BoolOr = BoolOr { getBoolOr :: Bool }
  deriving (Show)
instance Semigroup (BoolOr) where
  (BoolOr a) <> (BoolOr b) = BoolOr (a || b)
instance Monoid (BoolOr) where
  mempty = BoolOr False

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just v) f = f v

listBind :: [a] -> (a -> [b]) -> [b]
listBind [] _ = []
listBind xs f = concat $ map f xs

eitherBind :: Either a b -> (b -> Either a c) -> Either a c
eitherBind (Left a) _ = Left a
eitherBind (Right b) f = f b

arrowBind :: (r->a) -> (a -> (r->b)) -> (r->b)
arrowBind f g = \x -> g (f x) x

pairBind :: Semigroup r => (r,a) -> (a -> (r,b)) -> (r,b)
pairBind (r1,a) f = (r1 <> r2,b)
  where (r2,b) = f a

nameTag = getLine >>= return