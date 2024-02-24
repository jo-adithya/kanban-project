module Utils where

import Text.Read (readMaybe)
import Types

printGroups :: [Group] -> IO ()
printGroups groups = printGroupsHelper 1 groups
 where
  printGroupsHelper :: Int -> [Group] -> IO ()
  printGroupsHelper _ [] = return ()
  printGroupsHelper index groups = do
    let group = head groups
    putStrLn $ "  (" ++ show index ++ ") " ++ name group
    printGroupsHelper (index + 1) (tail groups)

updateListAtIndex :: [a] -> Int -> a -> [a]
updateListAtIndex list index newElement = take (index - 1) list ++ [newElement] ++ drop index list

getUserAction :: String -> [String] -> IO Int
getUserAction prompt actions = do
  putStrLn prompt >> printActions 1 actions
  input <- getUserInput $ printOptions (length actions)
  let action = readMaybe input :: Maybe Int
  case action of
    Just a
      | a `elem` [1 .. length actions] -> return a
      | otherwise -> do
          invalidAction
          getUserAction prompt actions
    Nothing -> do
      invalidAction
      getUserAction prompt actions
 where
  printActions :: Int -> [String] -> IO ()
  printActions _ [] = return ()
  printActions index (action : rest) = do
    putStrLn $ "  (" ++ show index ++ ") " ++ action
    printActions (index + 1) rest
  printOptions :: Int -> String
  printOptions 1 = "Choose an option (1): "
  printOptions n = "Choose an option (1-" ++ show n ++ "): "

getUserInput :: String -> IO String
getUserInput prompt = putStr prompt >> getLine >>= fixdel

invalidAction :: IO ()
invalidAction = putStrLn "Invalid action. Please choose a valid action.\n"

fixdel :: [Char] -> IO [Char]
fixdel string
  | '\DEL' `elem` string = fixdel (remdel string)
  | otherwise = return string

remdel :: [Char] -> [Char]
remdel ('\DEL' : rest) = rest
remdel (a : '\DEL' : rest) = rest
remdel (a : rest) = a : remdel rest
