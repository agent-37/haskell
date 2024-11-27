import Control.Monad.State
import System.Environment


next :: Char -> String
next 'a' = "ab"
next 'b' = "a"


generate :: Int -> (Char -> String) -> Char -> String
generate n f c = iterate (concatMap f) [c] !! n

gen :: Int -> String
gen n = generate n next 'a'  

main :: IO ()
main = do
    args <- getArgs
    let n = read (head args) :: Int
        res = gen n

    putStrLn res
