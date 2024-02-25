module DataS where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

-- Function to connect to the database and initialize the schema
initDB :: FilePath -> IO ()
initDB dbPath = do
    conn <- connectSqlite3 dbPath
    run conn "CREATE TABLE IF NOT EXISTS group_table (id INTEGER PRIMARY KEY, group_name TEXT)" []
    run conn "CREATE TABLE IF NOT EXISTS tasks_table (id INTEGER PRIMARY KEY, task_name TEXT, task_description TEXT, group_id INTEGER, FOREIGN KEY(group_id) REFERENCES group_table(id))" []
    commit conn
    disconnect conn

-- Insert a group into the database
insertGroupD :: FilePath -> String -> IO ()
insertGroupD dbPath groupName = do
    conn <- connectSqlite3 dbPath
    run conn "INSERT INTO group_table (group_name) VALUES (?)" [toSql groupName]
    commit conn
    disconnect conn

-- Insert a task into the database
insertTaskD :: FilePath -> String -> String -> Int -> IO ()
insertTaskD dbPath taskName taskDescription groupId = do
    conn <- connectSqlite3 dbPath
    run conn "INSERT INTO tasks_table (task_name, task_description, group_id) VALUES (?, ?, ?)" [toSql taskName, toSql taskDescription, toSql groupId]
    commit conn
    disconnect conn

-- Update a group into the database
updateGroupD :: FilePath -> Int -> String -> IO ()
updateGroupD dbPath groupId newName = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE group_table SET group_name = ? WHERE id = ?" [toSql newName, toSql groupId]
    commit conn
    disconnect conn

-- Update a task into the database
updateTaskD :: FilePath -> Int -> String -> String -> Int -> IO ()
updateTaskD dbPath taskId newTaskName newDescription newGroupId = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_name = ?, task_description = ?, group_id = ? WHERE id = ?" [toSql newTaskName, toSql newDescription, toSql newGroupId, toSql taskId]
    commit conn
    disconnect conn

updateTaskTitleD :: FilePath -> Int -> String -> IO ()
updateTaskTitleD dbPath taskId newTitle = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_name = ? WHERE id = ?" [toSql newTitle, toSql taskId]
    commit conn
    disconnect conn

updateTaskDescriptionD :: FilePath -> Int -> String -> IO ()
updateTaskDescriptionD dbPath taskId newDescription = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_description = ? WHERE id = ?" [toSql newDescription, toSql taskId]
    commit conn
    disconnect conn

-- Updates the group of a specific task in the database
updateTaskGroupD :: FilePath -> Int -> Int -> IO ()
updateTaskGroupD dbPath taskId newGroupId = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET group_id = ? WHERE id = ?" [toSql newGroupId, toSql taskId]
    commit conn
    disconnect conn

-- Deleting a group in the database
deleteGroupD :: FilePath -> Int -> IO ()
deleteGroupD dbPath groupId = do
    conn <- connectSqlite3 dbPath
    _ <- run conn "DELETE FROM tasks_table WHERE group_id = ?" [toSql groupId]
    _ <- run conn "DELETE FROM group_table WHERE id = ?" [toSql groupId]
    commit conn
    disconnect conn

-- Delete a task in the database
deleteTaskD :: FilePath -> Int -> IO ()
deleteTaskD dbPath taskId = do
    conn <- connectSqlite3 dbPath
    run conn "DELETE FROM tasks_table WHERE id = ?" [toSql taskId]
    commit conn
    disconnect conn

-- Fetch all groups
fetchGroupsD :: FilePath -> IO [(Int, String)]
fetchGroupsD dbPath = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id, group_name FROM group_table" []
    disconnect conn
    return $ map (\[sqlId, sqlName] -> (fromSql sqlId, fromSql sqlName)) res

-- Fetches a group's ID by its name
fetchGroupIdByNameD :: FilePath -> String -> IO (Maybe Int)
fetchGroupIdByNameD dbPath groupName = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id FROM group_table WHERE group_name = ? LIMIT 1" [toSql groupName]
    disconnect conn
    return $ case res of
        [[sqlId]] -> Just (fromSql sqlId)
        _ -> Nothing

-- Fetch all tasks
fetchTasksD :: FilePath -> IO [(Int, String, String, Int)]
fetchTasksD dbPath = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id, task_name, task_description, group_id FROM tasks_table" []
    disconnect conn
    return $ map (\[sqlId, sqlName, sqlDesc, sqlGroupId] -> (fromSql sqlId, fromSql sqlName, fromSql sqlDesc, fromSql sqlGroupId)) res

fetchTaskIdByNameD :: FilePath -> String -> IO (Maybe Int)
fetchTaskIdByNameD dbPath taskName = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id FROM tasks_table WHERE task_name = ? LIMIT 1" [toSql taskName]
    disconnect conn
    return $ case res of
        [[sqlId]] -> Just (fromSql sqlId)
        _ -> Nothing