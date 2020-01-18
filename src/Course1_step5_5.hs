module Course1_step5_5 where

import System.Directory
import Data.List (isInfixOf)

main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine 
    if name /= ""
        then putStrLn $ "Hi, " ++ name ++ "!"
        else main'
        

main'' :: IO ()
main'' = do
    putStr "Substring: "
    substring <- getLine
    if substring == "" 
    then putStrLn "Canceled"
    else do
        fileList <- getDirectoryContents "."
        let fileListFiltered = filter (isInfixOf substring) fileList
        mapM_ (\x -> putStrLn ("Removing file: " ++ x) >> removeFile x) fileListFiltered  