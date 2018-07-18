type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please input a name: "
  name <- getLine
  putStrLn "Please input an age: "
  age <- getLine
  let person = mkPerson name (read age) in
    case person of
      Right x -> do
        putStrLn "Yay! Successfully got a person:"
        putStrLn (show x)
      Left x -> do
        putStrLn "Error creating a person: "
        putStrLn (show x)
