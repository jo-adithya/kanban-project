module DataS where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

-- | Function to reset the database
--
-- Resets the database by deleting all records from the group_table and tasks_table,
-- and then initializes the database using 'initDB' function.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--
-- Examples:
-- >>> resetDB "kanban.db"
-- Database should be emtpy now
resetDB :: FilePath -> IO ()
resetDB dbPath = do
    conn <- connectSqlite3 dbPath
    run conn "DELETE FROM group_table" []
    run conn "DELETE FROM tasks_table" []
    commit conn
    disconnect conn
    initDB dbPath

-- | Function to connect to the database and initialize the schema
--
-- Initializes the database by creating necessary tables if they don't exist.
-- The function takes a 'FilePath' representing the path to the database file.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--
-- Examples:
-- ** Erase the database file before running the following example. **
-- >>> initDB "kanban.db"
-- Database should be initialized now
initDB :: FilePath -> IO ()
initDB dbPath = do
    conn <- connectSqlite3 dbPath
    run conn "CREATE TABLE IF NOT EXISTS group_table (id INTEGER PRIMARY KEY, group_name TEXT)" []
    run conn "CREATE TABLE IF NOT EXISTS tasks_table (id INTEGER PRIMARY KEY, task_name TEXT, task_description TEXT, group_id INTEGER, FOREIGN KEY(group_id) REFERENCES group_table(id))" []
    commit conn
    disconnect conn

-- | Insert a group into the database
--
-- The group name is provided as a parameter.
-- The function connects to the SQLite database specified by 'dbPath',
-- inserts the group name into the 'group_table' table,
-- and then commits the transaction and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   groupName: The name of the group to insert.
--
-- Examples:
-- >>> insertGroupD "kanban.db" "Group 1"
-- Group should be inserted now
insertGroupD :: FilePath -> String -> IO ()
insertGroupD dbPath groupName = do
    conn <- connectSqlite3 dbPath
    run conn "INSERT INTO group_table (group_name) VALUES (?)" [toSql groupName]
    commit conn
    disconnect conn

-- | Insert a task into the database
--
-- Takes the database file path, task name, task description, and group ID as parameters.
-- The function connects to the SQLite database specified by 'dbPath',
-- inserts the task name, task description, and group ID into the 'tasks_table' table,
-- and then commits the transaction and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   taskName: The name of the task to insert.
--   taskDescription: The description of the task to insert.
--   groupId: The ID of the group to which the task belongs.
--
-- Examples:
-- >>> insertTaskD "kanban.db" "Task 1" "Description 1" 1
-- Task should be inserted now
insertTaskD :: FilePath -> String -> String -> Int -> IO ()
insertTaskD dbPath taskName taskDescription groupId = do
    conn <- connectSqlite3 dbPath
    run conn "INSERT INTO tasks_table (task_name, task_description, group_id) VALUES (?, ?, ?)" [toSql taskName, toSql taskDescription, toSql groupId]
    commit conn
    disconnect conn

-- | Update a group into the database
--
-- Takes the path to the database file, the ID of the group to update, and the new name for the group.
-- The function connects to the SQLite database specified by 'dbPath',
-- updates the group name in the 'group_table' table,
-- and then commits the transaction and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   groupId: The ID of the group to update.
--   newName: The new name for the group.
--
-- Examples:
-- >>> updateGroupD "kanban.db" 1 "New Group 1"
-- Group should be updated now
updateGroupD :: FilePath -> Int -> String -> IO ()
updateGroupD dbPath groupId newName = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE group_table SET group_name = ? WHERE id = ?" [toSql newName, toSql groupId]
    commit conn
    disconnect conn

-- | Updates a task in the database with the specified task ID.
--
-- The function connects to the SQLite database using the provided file path,
-- updates the task with the given task ID in the 'tasks_table' by setting the
-- task name, description, and group ID to the provided values, and then
-- commits the changes and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   taskId: The ID of the task to update.
--   newTaskName: The new name for the task.
--   newDescription: The new description for the task.
--   newGroupId: The new group ID for the task.
--
-- Examples:
-- >>> updateTaskD "kanban.db" 1 "New Task 1" "New Description 1" 2
-- Task should be updated now
updateTaskD :: FilePath -> Int -> String -> String -> Int -> IO ()
updateTaskD dbPath taskId newTaskName newDescription newGroupId = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_name = ?, task_description = ?, group_id = ? WHERE id = ?" [toSql newTaskName, toSql newDescription, toSql newGroupId, toSql taskId]
    commit conn
    disconnect conn

-- | Updates a task title in the database with the specified task ID.
--
-- The function connects to the SQLite database using the provided file path,
-- updates the task with the given task ID in the 'tasks_table' by setting the
-- task title to the provided values, and then
-- commits the changes and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   taskId: The ID of the task to update.
--   newTitle: The new name for the task.
--
-- Examples:
-- >>> updateTaskTitleD "kanban.db" 1 "New Task 2"
-- Task should be updated now
updateTaskTitleD :: FilePath -> Int -> String -> IO ()
updateTaskTitleD dbPath taskId newTitle = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_name = ? WHERE id = ?" [toSql newTitle, toSql taskId]
    commit conn
    disconnect conn

-- | Updates a task description in the database with the specified task ID.
--
-- The function connects to the SQLite database using the provided file path,
-- updates the task with the given task ID in the 'tasks_table' by setting the
-- task description to the provided values, and then
-- commits the changes and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   taskId: The ID of the task to update.
--   newDescription: The new description for the task.
--
-- Examples:
-- >>> updateTaskDescriptionD "kanban.db" 1 "New Description 2"
-- Task should be updated now
updateTaskDescriptionD :: FilePath -> Int -> String -> IO ()
updateTaskDescriptionD dbPath taskId newDescription = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_description = ? WHERE id = ?" [toSql newDescription, toSql taskId]
    commit conn
    disconnect conn

-- | Updates the group of a specific task in the database
--
-- Takes the database file path, task ID, and new group ID as arguments.
-- The function connects to the SQLite database using the provided file path,
-- updates the task with the given task ID in the 'tasks_table' by setting the
-- group ID to the provided value, and then commits the changes and disconnects from the database.
--
-- Args: (Assumes all of the args are valid -> the task and group id exists)
--   dbPath: The path to the SQLite database file.
--   taskId: The ID of the task to update.
--   newGroupId: The new group ID for the task.
--
-- Examples:
-- >>> updateTaskGroupD "kanban.db" 1 1
-- Task should be updated now
updateTaskGroupD :: FilePath -> Int -> Int -> IO ()
updateTaskGroupD dbPath taskId newGroupId = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET group_id = ? WHERE id = ?" [toSql newGroupId, toSql taskId]
    commit conn
    disconnect conn

-- | Deletes a group and its associated tasks from the database.
--
-- Takes a 'FilePath' representing the path to the database file and an 'Int' representing the group ID.
-- The function connects to the SQLite database using the provided file path,
-- deletes all tasks associated with the group from the 'tasks_table' table,
-- deletes the group from the 'group_table' table, and then commits the changes and disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   groupId: The ID of the group to delete.
--
-- Examples:
-- >>> deleteGroupD "kanban.db" 1
-- Group should be deleted now
deleteGroupD :: FilePath -> Int -> IO ()
deleteGroupD dbPath groupId = do
    conn <- connectSqlite3 dbPath
    _ <- run conn "DELETE FROM tasks_table WHERE group_id = ?" [toSql groupId]
    _ <- run conn "DELETE FROM group_table WHERE id = ?" [toSql groupId]
    commit conn
    disconnect conn

-- | Deletes a task from the database.
--
-- Takes a 'FilePath' representing the path to the database file,
-- and an 'Int' representing the ID of the task to be deleted.
-- The function connects to the SQLite database using the provided file path,
-- deletes the task from the 'tasks_table' table, and then commits the changes and disconnects from the database.
--  
-- Args:
--   dbPath: The path to the SQLite database file.
--   taskId: The ID of the task to delete.
--
-- Examples:
-- >>> deleteTaskD "kanban.db" 1
-- Task should be deleted now
deleteTaskD :: FilePath -> Int -> IO ()
deleteTaskD dbPath taskId = do
    conn <- connectSqlite3 dbPath
    run conn "DELETE FROM tasks_table WHERE id = ?" [toSql taskId]
    commit conn
    disconnect conn

-- | Fetches the groups from the database and returns a list of tuples containing the group ID and group name.
--
-- The function connects to the SQLite database using the provided file path,
-- fetches all groups from the 'group_table' table, and then disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--
-- Returns:
--   A list of tuples containing the group ID and group name.
--
-- Examples:
-- >>> fetchGroupsD "kanban.db"
-- [ (2, "Group 2")]
fetchGroupsD :: FilePath -> IO [(Int, String)]
fetchGroupsD dbPath = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id, group_name FROM group_table" []
    disconnect conn
    return $ map (\[sqlId, sqlName] -> (fromSql sqlId, fromSql sqlName)) res

-- | Fetches the group ID by its name from the database.
--
-- The function connects to the SQLite database using the provided file path,
-- fetches the group ID from the 'group_table' table by its name, and then disconnects from the database.
--  
-- Args:
--   dbPath: The path to the SQLite database file.
--   groupName: The name of the group to fetch the ID for.
--
-- Returns:
--   'Just' the group ID if found, or 'Nothing' otherwise.
--
-- Examples:
-- >>> fetchGroupIdByNameD "kanban.db" "Group 2"
-- Just 2
fetchGroupIdByNameD :: FilePath -> String -> IO (Maybe Int)
fetchGroupIdByNameD dbPath groupName = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id FROM group_table WHERE group_name = ? LIMIT 1" [toSql groupName]
    disconnect conn
    return $ case res of
        [[sqlId]] -> Just (fromSql sqlId)
        _ -> Nothing

-- | Fetches tasks from the database.
--
-- The function connects to the SQLite database using the provided file path,
-- fetches all tasks from the 'tasks_table' table, and then disconnects from the database.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--
-- Returns:
--   A list of tuples containing the task ID, task name, task description, and group ID.
--
-- Examples: (Assumes that Task 1 is in DB)
-- >>> fetchTasksD "kanban.db"
-- [ (1, "Task 1", "Description 1", 1) ]
fetchTasksD :: FilePath -> IO [(Int, String, String, Int)]
fetchTasksD dbPath = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id, task_name, task_description, group_id FROM tasks_table" []
    disconnect conn
    return $ map (\[sqlId, sqlName, sqlDesc, sqlGroupId] -> (fromSql sqlId, fromSql sqlName, fromSql sqlDesc, fromSql sqlGroupId)) res

-- | Fetches the ID of a task by its name from the database.
--
-- Returns 'Just' the ID if the task is found, or 'Nothing' otherwise.
--
-- Args:
--   dbPath: The path to the SQLite database file.
--   taskName: The name of the task to fetch the ID for.
--
-- Examples:
-- >>> fetchTaskIdByNameD "kanban.db" "Task 1"
-- Just 1
fetchTaskIdByNameD :: FilePath -> String -> IO (Maybe Int)
fetchTaskIdByNameD dbPath taskName = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id FROM tasks_table WHERE task_name = ? LIMIT 1" [toSql taskName]
    disconnect conn
    return $ case res of
        [[sqlId]] -> Just (fromSql sqlId)
        _ -> Nothing