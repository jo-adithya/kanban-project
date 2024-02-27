module Helpers2 where

import Constants (dbPath)
import DataS (deleteGroupD, deleteTaskD, fetchGroupIdByNameD, fetchTaskIdByNameD, insertGroupD, insertTaskD, updateTaskDescriptionD, updateTaskGroupD, updateTaskTitleD)
import System.Exit (exitSuccess)
import Types (Group (..), State (..), Task (..))
import Utils (deleteListAtIndex, getUserAction, getUserInput, updateListAtIndex)

--------------- Create group ---------------

-- | createGroup function creates a new group in the state.
-- 
-- It prompts the user to enter the name of the group and checks if the group already exists.
-- If the group already exists, it prompts the user to choose a different name.
-- If the group doesn't exist, it inserts the group into the database, updates the state, and 
-- returns the updated state.
--
-- Args:
--   state: The current state.
--
-- Returns:
--   The updated state after creating a new group.
--
-- Example (1):
-- >>> createGroup (State [Group "Group 1"] [])
-- Creating a new group
-- -------------------
-- Enter the name of the group: Group 1
-- Group already exists. Please choose a different name.
--
-- Creating a new group
-- -------------------
-- Enter the name of the group: Group 2
-- Group created successfully!
--
-- Example (2):
-- >>> createGroup (State [] [])
-- Creating a new group
-- -------------------
-- Enter the name of the group: Group 1
-- Group created successfully!
-- 
-- 
-- User Input Format:
-- Desired name of the task group
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

-- | Creates a new task and updates the state accordingly.
--
-- If no groups exist, it asks the user to create a group first.
-- Otherwise, it prompts the user to enter the title and description of the task,
-- and then asks the user to select a group to add the task to.
-- The task is then inserted into the database and added to the state.
--
-- Args:
--   state: The current state.
--
-- Returns:
--   The updated state after creating a new task.
--
-- Example (1):
-- >>> createTask (State [] [])
-- No groups found. Please create a group first.
--
-- Example (2):
-- >>> createTask (State [Group "Group 1"] [])
-- Creating a new task
-- -------------------
-- Enter the title of the task: Task 1
-- Enter the description of the task:
-- Task 1 description
-- Which group do you want to add the task to?
--   (1) Group 1
-- Choose an option (1): 1
-- Task created successfully!
--
-- Example (3):
-- >>> createTask (State [Group "Group 1", Group "Group 2"] [])
-- Creating a new task
-- -------------------
-- Enter the title of the task: Task 2
-- Enter the description of the task:
-- Task 2 description
-- Which group do you want to add the task to?
--   (1) Group 1
--   (2) Group 2
-- Choose an option (1-2): 2
-- Task created successfully!
--
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
  groupId <- fetchGroupIdByNameD dbPath (show group)

  case groupId :: Maybe Int of
    Just groupId -> do
      insertTaskD dbPath title description groupId

  let newTask = Task title description group
  putStrLn "Task created successfully!\n\n"
  return $ State groups (tasks ++ [newTask]) -- This line might be replaced with updated state fetch logic

--------------- View all tasks ---------------

-- | Function to view all tasks in the state.
--
-- If there are no tasks, it prompts the user to go back ro exit.
-- If there are tasks, it prompts the user to select a task to view.
-- The user can also choose to go back or exit.
--
-- Args:
--   state: The current state.
--
-- Returns:
--   The updated state after viewing/editing tasks.
--
-- Example (1):
-- >>> viewAllTasks (State [] [])
-- No tasks found.
--
-- What do you want to do?
--   (1) Back
--   (2) Exit
-- Choose an option (1-2): 1
--
-- Example (2):
-- >>> viewAllTasks (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")])
-- Viewing all tasks
-- ------------------
--   (1) Task 1
--   (2) Back
--   (3) Exit
-- Choose an option (1-3): 1
--
-- Example (3):
-- >>> viewAllTasks (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1"), Task "Task 2" "Task 2 description" (Group "Group 2")])
-- Viewing all tasks
-- ------------------
--   (1) Task 1
--   (2) Task 2
--   (3) Back
--   (4) Exit
-- Choose an option (1-4): 2
--
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

-- | This function allows the user to view a specific task in the state.
--
-- It takes the current state and the index of the task as input.
-- It prints the task details and prompts the user for an action to perform on the task.
-- The available actions are: change title, change description, change group, delete task, go back, and exit.
-- Depending on the user's chosen action, it calls the corresponding action function and updates the state accordingly.
-- Finally, it recursively calls itself with the updated state and the same task index.
--
-- Args:
--   state: The current state.
--   index: The index of the task in the task list.
--
-- Returns:
--   The updated state after viewing/editing the task.
--
-- Example (1):
-- >>> viewTask (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) 1
-- Viewing a task
-- ----------------
-- Group: Group 1
-- Title: Task 1
-- Description: 
-- Task 1 description
--
-- What do you want to do?
--   (1) Change title
--   (2) Change description
--   (3) Change group
--   (4) Delete Task
--   (5) Back
--   (6) Exit
-- Choose an option (1-6): 5
--
-- Example (2):
-- >>> viewTask (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1"), Task "Task 2" "Task 2 description" (Group "Group 2")]) 2
-- Viewing a task
-- ----------------
-- Group: Group 2
-- Title: Task 2
-- Description:
-- Task 2 description
-- 
-- What do you want to do?
--   (1) Change title
--   (2) Change description
--   (3) Change group
--   (4) Delete Task
--   (5) Back
--   (6) Exit
-- Choose an option (1-6): 6
--
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

-- | Change the title of a task in the state.
-- 
-- If the task is found in the database, update its title and return the updated state.
-- Otherwise, return the original state.
--
-- Args:
--   state: The current state.
--   task: The task to update.
--   index: The index of the task in the task list.
--
-- Returns:
--   The updated state if the task is found in the database, otherwise the original state.
--
-- Example (1):
-- >>> changeTitle (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 1" "Task 1 description" (Group "Group 1")) 1
-- Changing the title of the task
-- ------------------------------
-- Enter the new title of the task: Task 2
-- Title changed successfully!

-- Example (2):
-- >>> changeTitle (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 1" "Task 1 description" (Group "Group 1")) 1
-- Changing the title of the task
-- ------------------------------
-- Enter the new title of the task: Task 2
-- Title changed successfully!
--
-- >>> changeTitle (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 1" "Task 1 description" (Group "Group 1")) 1
-- Changing the title of the task
-- ------------------------------
-- Enter the new title of the task: Task 2
-- Failed to find the task in the database.
--
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

-- | Change the description of a task in the state.
--
-- This function takes the current state, a task, and an index as input.
-- It prompts the user to enter a new description for the task and updates
-- the task's description in the database. If the task is found in the database,
-- the description is updated and the state is returned with the updated task.
-- If the task is not found, an error message is displayed and the original state is returned.
--
-- Args:
--   state: The current state.
--   task: The task to update.
--   index: The index of the task in the task list.
--
-- Returns:
--   The updated state if the task is found in the database, otherwise the original state.
--
-- Example (1):
-- >>> changeDescription (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 1" "Task 1 description" (Group "Group 1")) 1
-- Changing the description of the task
-- ------------------------------
-- Enter the new description of the task: 
-- Task 1 new description
-- Description changed successfully!

-- Example (2):
-- >>> changeDescription (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 2" "Task 1 description" (Group "Group 1")) 1
-- Changing the description of the task
-- ------------------------------
-- Enter the new description of the task: 
-- Task 1 new description
-- Failed to find the task in the database.
--
changeDescription :: State -> Task -> Int -> IO State
changeDescription state task index = do
  putStrLn "Changing the description of the task"
  putStrLn "------------------------------"
  newDescription <- getUserInput "Enter the new description of the task:\n"

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

-- | Change the group of a task in the state.
-- The function prompts the user to select a new group for the task,
-- updates the task's group in the database, and returns the updated state.
--
-- Args:
--   state: The current state.
--   task: The task to update.
--   index: The index of the task in the task list.
--
-- Returns:
--   The updated state after changing the group of the task.
--
-- Example (1):
-- >>> changeGroup (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 1" "Task 1 description" (Group "Group 1")) 1
-- Changing the group of the task
-- ------------------------------
-- Which group do you want to move the task to?
--   (1) Group 1
--   (2) Group 2
-- Choose an option (1-2): 2
-- Group changed successfully!
--
-- Example (2):
-- >>> changeGroup (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Task "Task 4" "Task 1 description" (Group "Group 1")) 1
-- Changing the group of the task
-- ------------------------------
-- Which group do you want to move the task to?
--   (1) Group 1
--   (2) Group 2
-- Choose an option (1-2): 2
-- Failed to find the task in the database.
--
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

-- | Deletes a task from the state based on the given index.
-- 
-- It also deletes the task from the database.
-- 
-- Args:
--   state: The current state.
--   index: The index of the task to delete. 
--          Requires index to be a valid index in the task list in the state.
--
-- Returns:
--  the updated state after deleting the task.

-- Example (1)
-- deleteTask (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) 1
-- Deleting a task
-- ----------------
-- Task deleted successfully!
--
-- Example (2) -- Assume that the state doesn't matches the database
-- deleteTask (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1"), Task "Task 2" "Task 2 description" (Group "Group 2")]) 1
-- Deleting a task
-- ----------------
-- Failed to find the task in the database.
--
deleteTask :: State -> Int -> IO State
deleteTask state index = do
  putStrLn "Deleting a task"
  putStrLn "----------------"

  let taskToDelete = tasks state !! (index - 1)
  taskId <- fetchTaskIdByNameD dbPath (title taskToDelete)

  case taskId :: Maybe Int of
    Just taskId -> do
      deleteTaskD dbPath taskId
      let newTasks = deleteListAtIndex (tasks state) index
      putStrLn "Task deleted successfully!\n"
      return $ State (groups state) newTasks
    Nothing -> do 
      putStrLn "Failed to find the task in the database."
      return state

--------------- Delete group ---------------

-- | Deletes a group from the state based on the given index.
--
-- It also deletes all tasks associated with the group.
-- 
-- Args:
--   state: The current state.
--   index: The index of the group to delete.
--          Requires index to be a valid index in the group list in the state.
--
-- Returns:
--  the updated state after deleting the group and its tasks.
--
-- Example (1)
-- deleteGroup (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) 1
-- Deleting a group
-- ----------------
-- Group and its tasks deleted successfully!
--
-- Returns: State [] []
--
-- Example (2) -- Assume that the state doesn't matches the database
-- deleteGroup (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1"), Task "Task 2" "Task 2 description" (Group "Group 2")]) 1
-- Deleting a group
-- ----------------
-- Failed to find the group in the database.
--
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
    Nothing -> do
      putStrLn "Failed to find the group in the database."
      return state

--------------- View all tasks by group ---------------

-- | Function to view all groups and their tasks in the state.
--
-- If no groups are found, it prompts the user to create a group first.
-- Otherwise, it prompts the user to select a group and then displays all tasks in that group.
-- Returns the updated state after viewing all groups.
--
-- Args:
--   state: The current state.
--
-- Returns:
--   The updated state after viewing all groups (probably some tasks are updated).
--
-- Example (1):
-- >>> viewAllGroups (State [] [])
-- No groups found. Please create a group first.
--
-- Example (2):
-- >>> viewAllGroups (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")])
-- Viewing all tasks by group
-- ---------------------------
-- Which group do you want to view?
--   (1) Group 1
-- Choose an option (1): 1
-- ** Calls viewAllTasksByGroup **
--
-- Example (3):
-- >>> viewAllGroups (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1"), Task "Task 2" "Task 2 description" (Group "Group 2")])
-- Viewing all tasks by group
-- ---------------------------
-- Which group do you want to view?
--   (1) Group 1
--   (2) Group 2
-- Choose an option (1-2): 2
-- ** Calls viewAllTasksByGroup **
--
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

-- | viewAllTasksByGroup is a function that takes a State and a Group as input and displays all tasks in the selected group.
--
-- If there are no tasks in the group, it prompts the user for further action.
-- The function returns an updated State if the user chooses to go back, or exits the program.
--
-- Args:
--   state: The current state.
--   selectedGroup: The group to view tasks for.
--
-- Returns:
--   The updated state after viewing all tasks in the selected group.
--
-- Example (1):
-- >>> viewAllTasksByGroup (State [Group "Group 1"] [Task "Task 1" "Task 1 description" (Group "Group 1")]) (Group "Group 1")
-- Viewing all tasks in Group 1 group
-- ---------------------------
--   (1) Task 1
--   (2) Back
--   (3) Exit
-- Choose an option (1-3): 1
-- ** Calls viewTask **
--
-- Example (2):
-- >>> viewAllTasksByGroup (State [Group "Group 1"] []) (Group "Group 1")
-- Viewing all tasks in Group 1 group
-- ---------------------------
-- No tasks found.
--
-- What do you want to do?
--   (1) Back
--   (2) Exit
-- Choose an option (1-2): 1
--
-- Example (3):
-- >>> viewAllTasksByGroup (State [Group "Group 1", Group "Group 2"] [Task "Task 1" "Task 1 description" (Group "Group 1"), Task "Task 2" "Task 2 description" (Group "Group 2")]) (Group "Group 2")
-- Viewing all tasks in Group 2 group
-- ---------------------------
--   (1) Task 2
--   (2) Back
--   (3) Exit
-- Choose an option (1-3): 2
--
viewAllTasksByGroup :: State -> Group -> IO State
viewAllTasksByGroup state selectedGroup = do
  -- Displaying the selected group
  putStrLn $ "Viewing all tasks in " ++ show selectedGroup ++ " group"
  putStrLn "---------------------------"
  
  -- Indexing the tasks and filtering based on the selected group
  let indexedTasks = zip [1 ..] (tasks state)
  let filteredTasks = filter (\(index, task) -> group task == selectedGroup) indexedTasks

  -- Handling the case when no tasks are found
  if null filteredTasks
    then do
      putStrLn "No tasks found.\n"
      let prompt = "What do you want to do?"
      let actions = ["Back", "Exit"]
      action <- getUserAction prompt actions
      case action of
        1 -> return state
        2 -> exitSuccess
    -- Handling the case when tasks are found
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
