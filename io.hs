import Control.Monad

main = forever $ do
    line <- getLine
    putStrLn $ reverse line