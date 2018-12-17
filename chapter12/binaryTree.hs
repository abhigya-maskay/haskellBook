import Data.Maybe

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f init = case f init of
  Nothing        -> Leaf
  Just(a1,a2,a3) -> Node (unfold f a1) a2 (unfold f a3)

treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n = unfold (\b -> if b==n then Nothing else Just(b+1,b,b+1)) 0
