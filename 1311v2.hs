import System.IO
import Data.Char
import System.Directory
import Data.List
import Control.Exception (catch, catches, Handler(..), IOException, SomeException, throwIO)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> loop fileName `catches` 
            [  Handler handleAlreadyInUse
            , Handler handleEOF
            , Handler handleSomeException
            ]
        _ -> putStrLn "Usage: <program> <filename>"

loop :: FilePath -> IO ()
loop fileName = do
    contents <- readFile fileName `catch` handleFileNotFound
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
            removeFile fileName
            renameFile tempName fileName 
            putStrLn "Task deleted."
            loop fileName
        
        "2" -> do
            putStrLn "Enter the new task:"
            newTask <- getLine
            let newTasks = tasks ++ [newTask]
            (tempName, tempHandle) <- openTempFile "." "temp"
            hPutStr tempHandle (unlines newTasks)
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName 
            putStrLn "Task added."
            loop fileName
            
        "q" -> putStrLn "Goodbye!"  
        
        _ -> do
            putStrLn "Invalid option."
            loop fileName  

handleFileNotFound :: IOException -> IO String
handleFileNotFound e = do
    putStrLn "File not found. Please ensure the specified file exists."
    throwIO e  -- Завершение программы с исключением

handleAlreadyInUse :: IOException -> IO ()
handleAlreadyInUse e = do
    putStrLn "The file is already in use. Please close it and try again."
    throwIO e  -- Завершение программы с исключением

handleEOF :: IOException -> IO ()
handleEOF e = do
    putStrLn "Reached the end of the file unexpectedly."
    throwIO e  -- Завершение программы с исключением

handleSomeException :: SomeException -> IO ()
handleSomeException e = do
    putStrLn $ "An error occurred: " ++ show e
    throwIO e  -- Завершение программы с исключением
