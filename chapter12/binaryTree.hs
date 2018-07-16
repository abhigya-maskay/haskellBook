data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f x =
  case (f x) of
    Just (x1, x2, x3) -> Node (unfold f x1) x2 (unfold f x3)
    Nothing -> Leaf

treeBuildHelper :: Integer -> Integer -> Maybe (Integer,Integer,Integer)
treeBuildHelper n x =
          case (x == n) of
            True -> Nothing
            False -> (Just (x+1,x,x+1))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (treeBuildHelper n) 0
