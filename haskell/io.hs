import Data.Char

firstExcercise = do
    putStrLn "What is your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

secondExcercise = do
    line <- getLine
    if null line
        then return()
        else do
            putStrLn $ reverseWords line
            secondExcercise

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main = do
    contents <- getContents
    if length contents > 10
        then putStrLn contents
        else main