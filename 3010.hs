import Data.Char
import Control.Monad
--main = print [1,2,3,4,5,4,3,2,1]

-- putStr
--putStrLn
--putChar

--getLine
--getContens

--main = do 
--  putStrLn "What is yor name?"
--  name <-getLine
--  putStrLn ("Hi, "++name++"!")


--main = do 
--  putStrLn "What is yor first name?"
--  first_name <-getLine
--  putStrLn "What is yor surname?"
--  sec_name <- getLine
--  let bfn = map toUpper first_name
--      bsn = map toUpper sec_name
--  putStrLn $ "Hi, "++bfn++" "++bsn++"!"

--main = do 
--  line <- getLine
--  if null line
--      then return ()
--      else do
--          putStr $ unwords . map reverse. words line
--          main

--main = do 
--  input <- getLine
--  when (input =="!") $ do
--      putStrLn input

--main = do 
--   rs <- sequence [getLine,getLine,getLine]
--   print rs

--main = do
--  sequence $ map print [1,2,3,4]


--main = do
--  mapM print [1,2,3,4]

--main = forever $ do
--  putStr "yyy"
--  i <-getLine
--  putStrLn $ map toUpper i

--main = do
--  res <-forM [1,2,3](\a -> do
--    print a
--    c <-getLine
--    return c)
--  mapM putStrLn res

--main = do
--  putStrLn "What number need to check?"
--  num <- getLine
--  let nnum =  ( read num ::Int)
--  --let count = 0
--  res <-forM [1,2,3,4,5,6,7,8,9](\a -> do
--    putStrLn (num++"*"++(show a)++"=?")
--    c <- getLine
--    let cc = ( read c ::Int)
--    --let aa = (read a ::Int) 
--    if nnum * a == cc
--    then putStrLn "True"
--    else putStrLn "False"
--    if nnum * a == cc
--    then return True
--    else return False)
--  res1 <- filterM (\x -> return x) res
--  putStrLn (  "Amount true answers:"++ (show (length res1)))
--  --mapM (\a -> putStrLn $ show a)  res

--main = do
--  putStrLn "a"
--  a <- getLine
--  let aa =  ( read a ::Double)
--  putStrLn "b"
--  b <- getLine
--  let bb =  ( read b ::Double)
--  putStrLn "c"
--  c <- getLine
--  let cc =  ( read c ::Double)
--  if bb * bb - 4 * aa * cc>=0
--  then putStrLn ("roots "++(show ((-bb+ sqrt (bb * bb - 4 * aa * cc))/(2*aa)))++" ; "++(show ((-bb - sqrt (bb * bb - 4 * aa * cc))/(2*aa))))
--  else putStrLn ("roots "++(show ((-bb)/(2*aa)))++"+"++ (show ((4*aa*cc-bb*bb)/(2*aa)))++"i"++" ; "++(show ((-bb)/(2*aa)))++"-"++ (show ((4*aa*cc-bb*bb)/(2*aa)))++"i") 

main = do
  putStrLn "l"
  l <- getLine
  let ll =  ( read l ::Double)
  putStrLn "r"
  r <- getLine
  let rr =  ( read r ::Double)
  putStrLn "dh"
  dh <- getLine
  let ddh =  ( read dh ::Double)
      ff x = x * x + 2 * x + 5
  print ([(ff i) | i <- [ll .. ((rr - ll) / ddh + 10)], ddh * i + ll <= rr])
