module Utils where

import Text.Read (readMaybe)
import Types

import System.Console.ANSI (clearScreen, setCursorPosition)

-- | Updates the element at the specified index in a list.
--
-- The function takes a list, an index, and a new element. It returns a new list
-- where the element at the specified index is replaced with the new element.
-- Requires the index to be a valid index in the list.
--
-- Args:
--   list: The list to update.
--   index: The index of the element to update.
--   newElement: The new element to replace the old element at the specified index.
--
-- Returns:
--   A new list with the element at the specified index replaced with the new element.
--
-- Examples:
--
-- >>> updateListAtIndex [1, 2, 3] 1 4
-- [4,2,3]
--
-- >>> updateListAtIndex [1, 2, 3] 2 4
-- [1,4,3]
--
-- >>> updateListAtIndex [1, 2, 3] 3 4
-- [1,2,4]
--
updateListAtIndex :: [a] -> Int -> a -> [a]
updateListAtIndex list index newElement = take (index - 1) list ++ [newElement] ++ drop index list

-- | Deletes an element from a list at the specified index.
--
-- Returns a new list with the element removed.
-- Requires the index to be a valid index in the list.
--
-- Examples:
--
-- >>> deleteListAtIndex [1,2,3,4,5] 2
-- [1,3,4,5]
--
-- >>> deleteListAtIndex [1,2,3,4,5] 5
-- [1,2,3,4]
--
-- >>> deleteListAtIndex [1,2,3,4,5] 1
-- [2,3,4,5]
--
deleteListAtIndex :: [a] -> Int -> [a]
deleteListAtIndex list index = take (index - 1) list ++ drop index list

-- | Prompts the user with a message and a list of actions, and returns the selected action index.
--
-- If the user enters an invalid input, it prompts again until a valid input is provided.
--
-- Args:
--   prompt: The message to display to the user.
--   actions: The list of actions to display to the user.
--
-- Returns:
--   The index of the selected action.
--
-- Examples:
--
-- >>> getUserAction "Choose an action:" ["Action 1", "Action 2", "Action 3"]
-- Choose an action:
--   (1) Action 1
--   (2) Action 2
--   (3) Action 3
-- Choose an option (1-3): 2
-- Returns 2
--
-- >>> getUserAction "Choose an action:" ["Action 1", "Action 2", "Action 3"]
-- Choose an action:
--   (1) Action 1
--   (2) Action 2
--   (3) Action 3
-- Choose an option (1-3): 4
-- Invalid action. Please choose a valid action.
-- Choose an action:
--   (1) Action 1
--   (2) Action 2
--   (3) Action 3
-- Choose an option (1-3): 3
-- Returns 3
--
getUserAction :: String -> [String] -> IO Int
getUserAction prompt actions = do
  putStrLn prompt >> printActions 1 actions
  input <- getUserInput $ printOptions (length actions)
  let action = readMaybe input :: Maybe Int
  case action of
    Just a
      | a `elem` [1 .. length actions] -> clearScreen >> setCursorPosition 0 0 >> return a
      | otherwise -> do
          invalidAction
          getUserAction prompt actions
    Nothing -> do
      invalidAction
      getUserAction prompt actions
 where
  -- | Prints the list of actions with their corresponding indices.
  printActions :: Int -> [String] -> IO ()
  printActions _ [] = return ()
  printActions index (action : rest) = do
    putStrLn $ "  (" ++ show index ++ ") " ++ action
    printActions (index + 1) rest
  
  -- | Returns the prompt for selecting an option based on the number of actions.
  printOptions :: Int -> String
  printOptions 1 = "Choose an option (1): "
  printOptions n = "Choose an option (1-" ++ show n ++ "): "

-- | Prompts the user with a message and retrieves their input from the command line.
--
-- The input is then processed to remove any trailing newline characters.
--
-- Args:
--   prompt: The message to display to the user.
--
-- Returns:
--   The user's input.
--
-- Examples:
--
-- >>> getUserInput "Enter a number: "
-- Enter a number: 5
-- Returns "5"
--
-- >>> getUserInput "Enter a number: \n"
-- Enter a number: 
-- 5
-- Returns "5"
--
getUserInput :: String -> IO String
getUserInput prompt = putStr prompt >> getLine >>= fixdel

-- | Prints an error message indicating that an invalid action was chosen.
--
-- Examples:
--
-- >>> invalidAction
-- Invalid action. Please choose a valid action.
--
invalidAction :: IO ()
invalidAction = putStrLn "Invalid action. Please choose a valid action.\n"


-- | This function takes a string as input and removes all occurrences of the backspace character ('\DEL') from the string.
--
-- It recursively calls itself until there are no more backspace characters in the string.
-- The resulting string is returned wrapped in the IO monad.
--
-- Args:
--   string: The string to remove backspace characters from.
--
-- Returns:
--   The string with all backspace characters removed.
--
-- Examples:
--
-- >>> fixdel "abc\DEL\DEL\DELdef"
-- "def"
--
-- >>> fixdel "abc\DEL"
-- "ab"
--
-- >>> fixdel "abc\DEL\DEL\DEL"
-- ""
--
fixdel :: [Char] -> IO [Char]
fixdel string
  | '\DEL' `elem` string = fixdel (remdel string)
  | otherwise = return string

-- | Removes the backspace character ('\DEL') and the character preceding it from a string.
--
-- If the string contains multiple backspace characters, it removes each pair of backspace and preceding character.
-- Returns the modified string.
--
-- Args:
--   string: The string to remove backspace characters from.
--
-- Returns:
--   The string with all backspace characters removed.
--
-- Examples:
-- 
-- >>> remdel "abc\DEL\DEL\DELdef"
-- "def"
--
-- >>> remdel "abc\DEL"
-- "ab"
--
-- >>> remdel "abc\DEL\DEL\DEL"
-- ""
--
remdel :: [Char] -> [Char]
remdel ('\DEL' : rest) = rest
remdel (a : '\DEL' : rest) = rest
remdel (a : rest) = a : remdel rest
