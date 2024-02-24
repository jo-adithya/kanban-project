module Kanban where

import Constants (dbPath)
import DataS (fetchGroups, fetchTasks, initDB)
import Helpers (createGroup, createTask, viewAllGroups, viewAllTasks)
import Text.Read (readMaybe)
import Types (Group (Group), State (State), Task (Task))
import Utils (getUserAction)

---------------- Main ------------------

app :: State -> IO ()
app state = do
  let prompt = "What do you want to do?"
  let actions = ["Create a new group", "Create a new task", "View all tasks", "View all tasks by group", "Exit"]
  action <- getUserAction prompt actions
  case action of
    1 -> createGroup state >>= app
    2 -> createTask state >>= app
    3 -> viewAllTasks state >>= app
    4 -> viewAllGroups state >>= app
    5 -> return ()

-- Main function
main :: IO ()
main = do
  putStrLn "Welcome to Kanban Haskell Project!\n"
  putStrLn "Loading the state from the database (kanban.db)...\n"

  -- Initialize the database
  initDB dbPath

  -- Load the state from database
  sqlGroups <- fetchGroups dbPath
  sqlTasks <- fetchTasks dbPath

  -- Convert the SQL result to State
  let groups = map (\(id, name) -> Group name) sqlGroups
  let tasks = map (\(id, name, desc, groupId) -> Task name desc (findGroupById sqlGroups groupId)) sqlTasks

  -- Start the app
  app (State groups tasks)

-- Find group by sql id helper
findGroupById :: [(Int, String)] -> Int -> Group
findGroupById groups id = do
  let filteredGroups = filter (\(groupId, _) -> groupId == id) groups
  let group = head filteredGroups
  Group (snd group)
