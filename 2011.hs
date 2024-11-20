import System.IO
import Data.List (intercalate)
import Control.Exception (catch, SomeException)

type Matrix = [[Char]]

readMatrix filePath = do
    content <- readFile filePath
    return $ lines content  

writeMatrix filePath matrix = writeFile filePath (unlines matrix)  

printMatrix matrix = mapM_ putStrLn matrix  

checkAndReplace matrix searchString row col direction = do
    let m = length matrix
        n = length (head matrix)
        found = case direction of
            'g' -> if col + length searchString <= n
                   then all (== True) [matrix !! row !! (col + i) == (searchString !! i) | i <- [0..length searchString - 1]]
                   else False
            'w' -> if row + length searchString <= m
                   then all (== True) [matrix !! (row + i) !! col == (searchString !! i) | i <- [0..length searchString - 1]]
                   else False
    if found
        then do
            let newMatrix = case direction of
                    'g' -> replaceHorizontally matrix row col (length searchString)
                    'w' -> replaceVertically matrix row col (length searchString)
            return (newMatrix, True)
        else return (matrix, False)

replaceHorizontally matrix row col length = 
    let (before, target:after) = splitAt row matrix
        newRow = take col target ++ replicate length '*' ++ drop (col + length) target  
    in before ++ newRow : after

replaceVertically matrix row col length = 
    let newMatrix = foldl (\m r -> replaceAt m r col '*') matrix [row..row + length - 1]
    in newMatrix

replaceAt matrix row col newChar =
    let (before, target:after) = splitAt row matrix
        newRow = replaceAtCol target col newChar
    in before ++ newRow : after

replaceAtCol x col newChar =
    let (before, _:after) = splitAt col x
    in before ++ [newChar] ++ after

isMatrixFilled = all (all (== '*'))

getValidPosition :: Int -> Int -> IO (Int, Int)
getValidPosition maxRows maxCols = do
    putStrLn "Введите позицию (две целых числа через пробел):"
    pos <- getLine
    let inputs = words pos
    if length inputs /= 2
        then do
            putStrLn "Ошибка: необходимо ввести два числа."
            getValidPosition maxRows maxCols
        else do
            let [rowStr, colStr] = inputs
            case (reads rowStr :: [(Int, String)], reads colStr :: [(Int, String)]) of
                ([(row, "")], [(col, "")]) -> 
                    if row < 0 || row >= maxRows || col < 0 || col >= maxCols
                        then do
                            putStrLn $ "Ошибка: числа должны быть в диапазоне от 0 до " ++ show (maxRows - 1) ++ " и от 0 до " ++ show (maxCols - 1) ++ "."
                            getValidPosition maxRows maxCols
                        else return (row, col)
                _ -> do
                    putStrLn "Ошибка: необходимо ввести два целых числа."
                    getValidPosition maxRows maxCols

getValidDirection :: IO Char
getValidDirection = do
    putStrLn "Введите символ направления ('g' или 'w'):"
    direction <- getLine
    if length direction /= 1 || (head direction /= 'g' && head direction /= 'w')
        then do
            putStrLn "Ошибка: необходимо ввести 'g' или 'w'."
            getValidDirection
        else return (head direction)

main :: IO ()
main = do
    let filePath = "input.txt"
    matrix <- readMatrix filePath
    printMatrix matrix
    let loop currentMatrix = do
            if isMatrixFilled currentMatrix
                then putStrLn "Поздравляем!"
                else do
                    direction <- getValidDirection
                    let maxRows = length currentMatrix
                    let maxCols = length (head currentMatrix)
                    (row, col) <- getValidPosition maxRows maxCols
                    putStrLn "Введите строку для поиска:"
                    searchString <- getLine
                    (newMatrix, found) <- checkAndReplace currentMatrix searchString row col direction
                    writeMatrix filePath newMatrix
                    printMatrix newMatrix
                    if found
                        then do
                            putStrLn "Строка заменена!"
                            loop newMatrix 
                        else do
                            putStrLn "Строка не найдена."
                            loop currentMatrix  
    loop matrix 
