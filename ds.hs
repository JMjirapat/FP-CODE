data LinkListed a = LinkNode a (LinkListed a) | LinkEmpty

instance (Show a) => Show (LinkListed a) where
    show LinkEmpty = ""
    show (LinkNode x LinkEmpty) = show x
    show (LinkNode x xs) = show x ++ " -> " ++ show xs

insertLinkListed :: a -> LinkListed a -> LinkListed a
insertLinkListed a LinkEmpty = LinkNode a LinkEmpty
insertLinkListed a (LinkNode x xs) = LinkNode a (LinkNode x xs)

searchLinkListed :: (Eq a) => a -> LinkListed a -> Maybe a
searchLinkListed _ LinkEmpty = Nothing
searchLinkListed x (LinkNode y xs) = if x == y then Just y else searchLinkListed x xs

deleteLinkListed :: (Eq a) => a -> LinkListed a -> LinkListed a
deleteLinkListed _ LinkEmpty = LinkEmpty
deleteLinkListed x (LinkNode y xs)
    | x == y    = xs
    | otherwise = LinkNode y (deleteLinkListed x xs)

type Stack a = [a]

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> (Maybe a, Stack a)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

searchStack :: (Eq a) => a -> Stack a -> Maybe a
searchStack _ [] = Nothing
searchStack x (y:ys) = if x == y then Just y else searchStack x ys

type Queue a = [a]

enqueue :: a -> Queue a -> Queue a
enqueue x = (++[x])

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue [] = (Nothing, [])
dequeue (x:xs) = (Just x, xs)

searchQueue :: (Eq a) => a -> Queue a -> Maybe a
searchQueue _ [] = Nothing
searchQueue x (y:ys) = if x == y then Just y else searchQueue x ys
