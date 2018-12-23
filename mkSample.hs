main = do
    l <- getContents
    let a:b:[] = lines l
    putStrLn $ head $ words a
    mapM_ putStrLn $ take 4 $ words b
