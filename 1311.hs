import System.IO
import Data.Char
import System.Directory
import Data.List
import Control.Exception (catch, catches, Handler(..), IOException, SomeException)

handleFileNotFound :: IOException -> IO String
handleFileNotFound e = do
    putStrLn "File not found. Please ensure 'test.txt' exists."
    return ""

handleAlreadyInUse :: IOException -> IO ()
handleAlreadyInUse e = putStrLn "The file is already in use. Please close it and try again."

handleEOF :: IOException -> IO ()
handleEOF e = putStrLn "Reached the end of the file unexpectedly."

handleSomeException :: SomeException -> IO ()
handleSomeException e = putStrLn $ "An error occurred: " ++ show e


main :: IO ()
main = do
    loop `catches` 
        [  Handler handleAlreadyInUse, Handler handleEOF, Handler handleSomeException]

loop :: IO ()
loop = do
    contents <- readFile "test.txt" `catch` handleFileNotFound
    let tasks = lines contents
        numTasks = zipWith (\n line -> (show n) ++ " - " ++ line) [1..] tasks
    
    putStrLn "Your tasks:"
    putStrLn $ unlines numTasks

    putStrLn "Choose an option: (1) Delete a task (2) Add a task (q) Quit"
    option <- getLine
    
    case option of
        "1" -> do
            putStrLn "Enter the number of the task to delete:"
            numStr <- getLine
            let num = read numStr
                newTasks = delete (tasks !! (num - 1)) tasks
            (tempName, tempHandle) <- openTempFile "." "temp"
            hPutStr tempHandle (unlines newTasks)
            hClose tempHandle
            removeFile "test.txt"
            renameFile tempName "test.txt" 
            putStrLn "Task deleted."
            loop
        
        "2" -> do
            putStrLn "Enter the new task:"
            newTask <- getLine
            let newTasks = tasks ++ [newTask]
            (tempName, tempHandle) <- openTempFile "." "temp"
            hPutStr tempHandle (unlines newTasks)
            hClose tempHandle
            removeFile "test.txt"
            renameFile tempName "test.txt" 
            putStrLn "Task added."
            loop
            
        "q" -> putStrLn "Goodbye!"  
        
        _ -> do
            putStrLn "Invalid option."
            loop  

