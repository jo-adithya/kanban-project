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
insertGroup :: FilePath -> String -> IO ()
insertGroup dbPath groupName = do
    conn <- connectSqlite3 dbPath
    run conn "INSERT INTO group_table (group_name) VALUES (?)" [toSql groupName]
    commit conn
    disconnect conn

-- Insert a task into the database
insertTask :: FilePath -> String -> String -> Int -> IO ()
insertTask dbPath taskName taskDescription groupId = do
    conn <- connectSqlite3 dbPath
    run conn "INSERT INTO tasks_table (task_name, task_description, group_id) VALUES (?, ?, ?)" [toSql taskName, toSql taskDescription, toSql groupId]
    commit conn
    disconnect conn

-- Update a group into the database
updateGroup :: FilePath -> Int -> String -> IO ()
updateGroup dbPath groupId newName = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE group_table SET group_name = ? WHERE id = ?" [toSql newName, toSql groupId]
    commit conn
    disconnect conn

-- Update a task into the database
updateTask :: FilePath -> Int -> String -> String -> Int -> IO ()
updateTask dbPath taskId newTaskName newDescription newGroupId = do
    conn <- connectSqlite3 dbPath
    run conn "UPDATE tasks_table SET task_name = ?, task_description = ?, group_id = ? WHERE id = ?" [toSql newTaskName, toSql newDescription, toSql newGroupId, toSql taskId]
    commit conn
    disconnect conn

-- Deleting a group in the database
deleteGroup :: FilePath -> Int -> IO ()
deleteGroup dbPath groupId = do
    conn <- connectSqlite3 dbPath
    run conn "DELETE FROM group_table WHERE id = ?" [toSql groupId]
    -- Optionally, delete tasks associated with this group or handle them differently
    commit conn
    disconnect conn

-- Delete a task in the database
deleteTask :: FilePath -> Int -> IO ()
deleteTask dbPath taskId = do
    conn <- connectSqlite3 dbPath
    run conn "DELETE FROM tasks_table WHERE id = ?" [toSql taskId]
    commit conn
    disconnect conn

-- Fetch all groups
fetchGroups :: FilePath -> IO [(Int, String)]
fetchGroups dbPath = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id, group_name FROM group_table" []
    disconnect conn
    return $ map (\[sqlId, sqlName] -> (fromSql sqlId, fromSql sqlName)) res

-- Fetch all tasks
fetchTasks :: FilePath -> IO [(Int, String, String, Int)]
fetchTasks dbPath = do
    conn <- connectSqlite3 dbPath
    res <- quickQuery' conn "SELECT id, task_name, task_description, group_id FROM tasks_table" []
    disconnect conn
    return $ map (\[sqlId, sqlName, sqlDesc, sqlGroupId] -> (fromSql sqlId, fromSql sqlName, fromSql sqlDesc, fromSql sqlGroupId)) res
