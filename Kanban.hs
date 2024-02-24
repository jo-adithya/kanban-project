module Kanban where

import Helpers (createGroup, createTask, listAllTasks)
import Text.Read (readMaybe)
import Types
import Utils (getUserAction)

---------------- Main ------------------

app :: State -> IO ()
app state = do
  let prompt = "What do you want to do?"
  let actions = ["Create a new group", "Create a new task", "List all tasks", "Exit"]
  action <- getUserAction prompt actions
  case action of
    1 -> createGroup state >>= app
    2 -> createTask state >>= app
    3 -> listAllTasks state >> app state
    4 -> return ()

main :: IO ()
main = do
  putStrLn "Welcome to Kanban Haskell Project!\n\n"
  app (State [])
