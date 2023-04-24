import Control.Monad (replicateM)
import System.Random (randomIO, randomRIO)

stringLength = 100

numStrings = 20

fileName = "./strings.txt"

charRange = ('a', 'z')

randomString :: Int -> IO String
randomString len = replicateM len $ randomRIO charRange

printToFile :: [String] -> IO ()
printToFile lst = do
    let contents = unlines lst

    writeFile fileName contents

testFunc :: Int -> Int -> String
testFunc n1 n2 = show n1 ++ show n2

main :: IO ()
main = do
    strings <- replicateM numStrings $ randomString stringLength
    printToFile strings
