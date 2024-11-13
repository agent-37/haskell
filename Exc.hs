import System.IO
import Data.Char
import System.Directory
import Data.List
import Control.Exception
  
handleArith :: ArithException -> IO()
handleArith _ = putStrLn "Del on 0!!!"

handlePattern :: PatternMatchFail -> IO()
handlePattern _ = putStrLn "Wrong args"

handleSome :: SomeException -> IO()
handleSome e = putStrLn (show e)


handleFile :: IOException -> IO()
handleFile e
    | isDoesNotExistsFile e = putStrLn "d"
    |isAlreadyExistsError e = putStrLn "d"
    |isDoesNotExistError  e = putStrLn "d"
    |isAlreadyInUseError  e = putStrLn "d"
    |isFullError  e = putStrLn "d"
    |isEOFError  e = putStrLn "d"
    |isIllegalOperation e = putStrLn "d"
    |isPermissionError e = putStrLn "d"
    |isUserError  e = putStrLn "d"
    |isResourceVanishedError e = putStrLn "d"

action a b = do
    print $ a `div` b
    print $ b `div` a

main = do
    let a = 5
    let b = 0
    res <- try( print $ a `div` b) :: IO (Either ArithException ())
    case res of
	    Left e -> putStrLn "Del on 0!!!"
	    Right () -> putStrLn "OK"		
    putStrLn "END"
    
    (action 0 5) `catch` handleArith
	(action 0 5) `catches`  [ Handler handleArith, Handler handlePattern, Handler handleSome]
	
	
   
   
