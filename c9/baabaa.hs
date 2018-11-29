import           System.IO

main :: IO ()
main = withFile "baabaa.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents
