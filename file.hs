import System.IO
import Data.Char
import System.Directory
import Data.List
import Control.Exception
   
   
main = do
   contents <- readFile  "test.txt" 
   let tasks = lines contents
       numTasks = zipWith (\n line -> (show n) ++ " - " ++ line ) [1, 2..] tasks
   putStrLn "Your tasks:"
   putStrLn $ unlines numTasks
   putStrLn "№ to del?"
   numStr <- getLine
   let num = read numStr
       newTasks = delete (tasks !! (num - 1)) tasks

   bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle)  -> do
           hClose tempHandle
		   removeFile tempName)
		   
      (\(tempName, tempHandle)  -> do	  
           hPutStr tempHandle (unlines newTasks)
           hClose tempHandle
           removeFile "test.txt"
           renameFile tempName "test.txt")


   
   
  
   
   
   
