module Types where

newtype State = State [Group]
  deriving (Show, Eq)

data Group = Group {name :: String, tasks :: [Task]}
  deriving (Eq)

instance Show Group where
  show :: Group -> String
  show (Group name tasks) = name ++ " (" ++ show (length tasks) ++ " tasks)" ++ "\n" ++ helper 1 tasks
   where
    helper :: Int -> [Task] -> String
    helper _ [] = ""
    helper index (task : rest) =
      "  (" ++ show index ++ ") " ++ title task ++ "\n" ++ helper (index + 1) rest

data Task = Task {title :: String, description :: String}
  deriving (Eq)

instance Show Task where
  show :: Task -> String
  show task = "Title: " ++ title task ++ "Description:\n" ++ description task
