module Helpers where

import Constants (dbPath)
import DataS (insertGroup)
import System.Exit (exitSuccess)
import Types (Group (..), State (..), Task (..))
import Utils (deleteListAtIndex, getUserAction, getUserInput, updateListAtIndex)

--------------- Create group ---------------

createGroup :: State -> IO State
createGroup (State groups tasks) = do
  putStrLn "Creating a new group"
  putStrLn "-------------------"
  name <- getUserInput "Enter the name of the group: "
  if name `elem` map (\(Group name) -> name) groups
    then do
      putStrLn "Group already exists. Please choose a different name."
      createGroup (State groups tasks)
    else do
      insertGroupD dbPath name
      putStrLn "Group created successfully!\n\n"
      return $ State (groups ++ [Group name]) tasks

--------------- Create task ---------------

import DataS (insertTask, fetchGroupIdByName)

createTask :: State -> IO State
createTask (State [] tasks) = do
  putStrLn "No groups found. Please create a group first.\n"
  return (State [] tasks)
createTask (State groups tasks) = do
  putStrLn "Creating a new task"
  putStrLn "-------------------"

  title <- getUserInput "Enter the title of the task: "
  description <- getUserInput "Enter the description of the task:\n"

  let prompt = "Which group do you want to add the task to?"
  let groupNames = map (\(Group name) -> name) groups
  groupIndex <- getUserAction prompt groupNames
  let group = groups !! (groupIndex - 1)
  groupId <- fetchGroupIdByName dbPath (show group)
  insertTaskD dbPath title description groupId

  putStrLn "Task created successfully!\n\n"
  return $ State groups (tasks ++ [newTask]) -- This line might be replaced with updated state fetch logic


--------------- View all tasks ---------------

viewAllTasks :: State -> IO State
viewAllTasks (State groups []) = do
  putStrLn "Viewing all tasks"
  putStrLn "------------------"
  putStrLn "No tasks found.\n"
  let prompt = "What do you want to do?"
  let actions = ["Back", "Exit"]
  action <- getUserAction prompt actions
  case action of
    1 -> return (State groups [])
    2 -> exitSuccess
viewAllTasks state = do
  putStrLn "Viewing all tasks"
  putStrLn "------------------"
  let prompt = "Which task do you want to view?"
  let actions = map title (tasks state) ++ ["Back", "Exit"]
  action <- getUserAction prompt actions

  let lengthTasks = length (tasks state)
  case action of
    n
      | n <= lengthTasks -> viewTask state n >>= viewAllTasks
      | n == lengthTasks + 1 -> return state
      | n == lengthTasks + 2 -> exitSuccess

--------------- View task ---------------

viewTask :: State -> Int -> IO State
viewTask state index = do
  putStrLn "Viewing a task"
  putStrLn "----------------"
  let task = tasks state !! (index - 1)
  print task
  putStrLn "\n"

  let prompt = "What do you want to do?"
  let actions = ["Change title", "Change description", "Change group", "Delete Task", "Back", "Exit"]
  action <- getUserAction prompt actions
  case action of
    1 -> handleAction changeTitle state task index
    2 -> handleAction changeDescription state task index
    3 -> handleAction changeGroup state task index
    4 -> deleteTask state index
    5 -> return state
    6 -> exitSuccess
 where
  handleAction :: (State -> Task -> Int -> IO State) -> State -> Task -> Int -> IO State
  handleAction actionFn state task index = do
    newState <- actionFn state task index
    viewTask newState index

--------------- Change title ---------------

changeTitle :: State -> Task -> Int -> IO State
changeTitle state task index = do
  putStrLn "Changing the title of the task"
  putStrLn "------------------------------"
  newTitle <- getUserInput "Enter the new title of the task: "

  taskIdMaybe <- fetchTaskIdByNameD dbPath (title task)
  
  case taskIdMaybe of
    Just taskId -> do
      updateTaskTitleD dbPath taskId newTitle
      
      let newTask = Task newTitle (description task) (group task)
      let newTasks = updateListAtIndex (tasks state) index newTask
      putStrLn "Title changed successfully!\n"
      return $ State (groups state) newTasks
      
    Nothing -> do
      putStrLn "Failed to find the task in the database."
      return state

--------------- Change description ---------------

changeDescription :: State -> Task -> Int -> IO State
changeDescription state task index = do
  putStrLn "Changing the description of the task"
  putStrLn "------------------------------"
  newDescription <- getUserInput "Enter the new description of the task: "

  taskIdMaybe <- fetchTaskIdByNameD dbPath (title task)
  
  case taskIdMaybe of
    Just taskId -> do
      updateTaskDescriptionD dbPath taskId newDescription
      
      let newTask = Task (title task) newDescription (group task)
      let newTasks = updateListAtIndex (tasks state) index newTask
      putStrLn "Description changed successfully!\n"
      return $ State (groups state) newTasks
      
    Nothing -> do
      putStrLn "Failed to find the task in the database."
      return state

--------------- Change group ---------------

changeGroup :: State -> Task -> Int -> IO State
changeGroup state task index = do
  putStrLn "Changing the group of the task"
  putStrLn "------------------------------"
  let prompt = "Which group do you want to move the task to?"
  let groupNames = map (\(Group name) -> name) (groups state)
  groupIndex <- getUserAction prompt groupNames
  let newGroup = groups state !! (groupIndex - 1)

  newGroupIdMaybe <- fetchGroupIdByNameD dbPath (show newGroup)

  case newGroupIdMaybe of
    Just newGroupId -> do
      taskIdMaybe <- fetchTaskIdByNameD dbPath (title task)
      case taskIdMaybe of
        Just taskId -> do
          updateTaskGroupD dbPath taskId newGroupId

          let newTask = Task (title task) (description task) newGroup
          let newTasks = updateListAtIndex (tasks state) index newTask
          putStrLn "Group changed successfully!\n"
          return $ State (groups state) newTasks

        Nothing -> do
          putStrLn "Failed to find the task in the database."
          return state

    Nothing -> do
      putStrLn "Failed to find the new group in the database."
      return state

--------------- Delete task ---------------

deleteTask :: State -> Int -> IO State
deleteTask state index = do
  putStrLn "Deleting a task"
  putStrLn "----------------"
  
  let taskToDelete = tasks state !! (index - 1)
  let taskId = fetchTaskIdByNameD dbPath (title task)

  deleteTaskD dbPath taskId
  
  let newTasks = deleteListAtIndex (tasks state) index
  putStrLn "Task deleted successfully!\n"
  return $ State (groups state) newTasks

--------------- Delete group ---------------

deleteGroup :: State -> Int -> IO State
deleteGroup state index = do
  putStrLn "Deleting a group"
  putStrLn "----------------"
  let Group groupName = groups state !! (index - 1)
  groupIdMaybe <- fetchGroupIdByNameD dbPath groupName

  case groupIdMaybe of
    Just groupId -> do
      deleteGroupD dbPath groupId
    
      let newGroups = deleteListAtIndex (groups state) index
      putStrLn "Group and its tasks deleted successfully!\n"
      return $ State newGroups (filter (\task -> group task /= Group groupName) (tasks state))

--------------- View all tasks by group ---------------

viewAllGroups :: State -> IO State
viewAllGroups (State [] tasks) = do
  putStrLn "No groups found. Please create a group first.\n"
  return (State [] tasks)
viewAllGroups state = do
  putStrLn "Viewing all tasks by group"
  putStrLn "---------------------------"
  let prompt = "Which group do you want to view?"
  let groupNames = map (\(Group name) -> name) (groups state)
  groupIndex <- getUserAction prompt groupNames

  let group = groups state !! (groupIndex - 1)
  viewAllTasksByGroup state group

viewAllTasksByGroup :: State -> Group -> IO State
viewAllTasksByGroup state selectedGroup = do
  putStrLn $ "Viewing all tasks in " ++ show selectedGroup ++ " group"
  putStrLn "---------------------------"
  let indexedTasks = zip [1 ..] (tasks state)
  let filteredTasks = filter (\(index, task) -> group task == selectedGroup) indexedTasks

  if null filteredTasks
    then do
      putStrLn "No tasks found.\n"
      let prompt = "What do you want to do?"
      let actions = ["Back", "Exit"]
      action <- getUserAction prompt actions
      case action of
        1 -> return state
        2 -> exitSuccess
    else do
      let prompt = "Which task do you want to view?"
      let actions = map (\(_, task) -> title task) filteredTasks ++ ["Back", "Exit"]
      action <- getUserAction prompt actions

      let lengthTasks = length filteredTasks
      case action of
        n
          | n <= lengthTasks -> do
              let index = fst (filteredTasks !! (n - 1))
              newState <- viewTask state index
              viewAllTasksByGroup newState selectedGroup
          | n == lengthTasks + 1 -> return state
          | n == lengthTasks + 2 -> exitSuccess