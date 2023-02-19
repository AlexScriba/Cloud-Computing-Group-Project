import Control.Monad (replicateM)
import System.Random (randomIO, randomRIO)

stringLength = 100

numStrings = 1000

fileName = "./strings.txt"

randomString :: Int -> IO String
randomString len = replicateM len $ randomRIO ('a', 'z')

printToFile :: [String] -> IO ()
printToFile lst = do
    let contents = unlines lst

    writeFile fileName contents

main :: IO ()
main = do
    strings <- replicateM numStrings $ randomString stringLength
    printToFile strings