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

xList = LinkNode 1 (LinkNode 2 (LinkNode 3 LinkEmpty))