import Control.Monad.State
import System.Environment


change :: String -> String
change = concatMap change_ch
  where
    change_ch 'a' = "ab"
    change_ch 'b' = "a"


gen_s :: Int -> State String ()
gen_s 0 = return ()
gen_s n = do
    current <- get
    let newString = change current
    put newString
    gen_s (n - 1)


gen :: Int -> String
gen n = execState (gen_s n) "a"

main :: IO ()
main = do
    args <- getArgs
    let n = read (head args) :: Int
        res = gen n
    putStrLn res
