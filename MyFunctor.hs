import BinSearchTree

class MyFunctor f where
	fmap2 ::  (a -> b) -> f a -> f b

instance MyFunctor Maybe where
	fmap2 f (Just x) = Just (f x)
	fmap2 f Nothing = Nothing

instance MyFunctor BinSearchTree.Tree where 
	fmap2 f EmptyTree = EmptyTree
	fmap2 f (Node a left right) =  Node (f a) (fmap2 f left) (fmap2 f right)

instance MyFunctor (Either Int) where
	fmap2 f (Left x) = Left x
	fmap2 f (Right x) = Right (f x)