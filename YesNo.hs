import BinSearchTree

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo (Maybe a) where
	yesno (Just _) = True
	yesno Nothing = False

instance YesNo (BinSearchTree.Tree a) where
	yesno BinSearchTree.EmptyTree = False
	yesno (BinSearchTree.Node _ _ _) = True


yesnoif :: (YesNo y) => y -> a -> a -> a
yesnoif yesnoVal yesRes noRes = 
	if yesno yesnoVal then yesRes 
	else noRes 

