module Helpers where

import System.Exit (exitSuccess)
import Types (Group (..), State (..), Task (..))
import Utils (getUserAction, getUserInput, printGroups, updateListAtIndex)

--------------- Create group ---------------

createGroup :: State -> IO State
createGroup (State groups) = do
  putStrLn "\n\nCreating a new group"
  putStrLn "-------------------"
  name <- getUserInput "Enter the name of the group: "
  let newGroup = Group name []
  putStrLn "Group created successfully!\n"
  return $ State (newGroup : groups)

--------------- Create task ---------------

createTask :: State -> IO State
createTask (State []) = do
  putStrLn "\nNo groups found. Please create a group first.\n"
  return (State [])
createTask state = do
  putStrLn "\n\nCreating a new task"
  putStrLn "-------------------"
  title <- getUserInput "Enter the title of the task: "
  description <- getUserInput "Enter the description of the task:\n"
  newState <- addTaskToGroup state (Task title description)
  putStrLn "Task created successfully!\n"
  return newState

addTaskToGroup :: State -> Task -> IO State
addTaskToGroup (State groups) task = do
  let prompt = "Which group do you want to add the task to?"
  let groupsName = map name groups
  groupIndex <- getUserAction prompt groupsName

  let group = groups !! (groupIndex - 1)
  let newGroup = Group (name group) (task : tasks group)
  let newGroups = updateListAtIndex groups groupIndex newGroup

  return $ State newGroups

--------------- List all tasks ---------------

listAllTasks :: State -> IO ()
listAllTasks state = do
  putStrLn "\n\nViewing all tasks"
  putStrLn "------------------"
  let tasks = getAllTasks state
  if null tasks
    then handleNoTasks
    else do
      listTasksHelper 1 tasks

  putStrLn "\n"
 where
  listTasksHelper :: Int -> [Task] -> IO ()
  listTasksHelper _ [] = return ()
  listTasksHelper index tasks = do
    let task = head tasks
    putStrLn $ "  (" ++ show index ++ ") " ++ title task
    listTasksHelper (index + 1) (tail tasks)

getAllTasks :: State -> [Task]
getAllTasks (State groups) = concatMap tasks groups

handleNoTasks :: IO ()
handleNoTasks = do
  putStrLn "No tasks found.\n"
  let prompt = "What do you want to do?"
  let actions = ["Back", "Exit"]
  action <- getUserAction prompt actions
  case action of
    1 -> return ()
    2 -> exitSuccess
