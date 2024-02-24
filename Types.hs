module Types where

data State = State {groups :: [Group], tasks :: [Task]}
  deriving (Show, Eq)

newtype Group = Group String
  deriving (Eq)

instance Show Group where
  show :: Group -> String
  show (Group name) = name

data Task = Task {title :: String, description :: String, group :: Group}
  deriving (Eq)

instance Show Task where
  show :: Task -> String
  show task = "Group: " ++ show (group task) ++ "\n" ++ "Title: " ++ title task ++ "\nDescription:\n" ++ description task
