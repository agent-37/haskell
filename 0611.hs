import System.IO
import Data.Char
import System.Directory
import Data.List
--main = do
--  withFile "text.txt" ReadMode(\handle -> do
--  	  contents <-hGetContents handle
--  	  putStr contents
--  	  )

--main = do 
--	handle <- openFile
--	"test.txt"  ReadMode
--	contents <- hGetContents
--	putStr contents
--	hClose handle

--main =do
--	contents <-readFile "test.txt"
--	writeFile "test1.txt" (map toUpper contents)

--main =do
--	contents <-readFile "test.txt"
--	writeFile "test1.txt" (map toUpper contents)
--	appendFile "test.txt" "ggggggg"

--main = do
--	contents <- readFile "test.txt"
--	let tasks = unlines $ zipWith(\n line-> (show n)++" - "++line) [1,2..] (lines contents)
--	putStr tasks


import System.IO
import Data.Char
import System.Directory
import Data.List
main = do
	loop

loop = do
    contents <- readFile "test.txt"
    let tasks = lines contents
        numTasks = zipWith (\n line -> (show n) ++ " - " ++ line) [1..] tasks
    
    putStrLn "Your tasks:"
    putStrLn $ unlines numTasks

    putStrLn "Choose an option: (1) Delete a task (2) Add a task"
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
