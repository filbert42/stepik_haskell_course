module Course1_step5_5 where

main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine 
    if name /= ""
        then putStrLn $ "Hi, " ++ name ++ "!"
        else main'
        
    