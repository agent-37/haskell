import System.IO
import Data.List (intercalate)

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

main :: IO ()
main = do
    let filePath = "input.txt"
    matrix <- readMatrix filePath
    printMatrix matrix
    let loop currentMatrix = do
            if isMatrixFilled currentMatrix
                then putStrLn "Поздравляем!"
                else do
                    putStrLn "Введите символ:"
                    direction <- getLine
                    putStrLn "Введите позицию:"
                    pos <- getLine
                    let [rowStr, colStr] = words pos
                        row = read rowStr
                        col = read colStr
                    putStrLn "Введите строку для поиска:"
                    searchString <- getLine
                    let dir = head direction
                    (newMatrix, found) <- checkAndReplace currentMatrix searchString row col dir
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
