import System.IO
import System.Random
import Data.Char

main :: IO ()
main = do
        contents <- readFile "resources/palavras.txt"
        let palavras = lines contents
        n <- randomRIO (0, 12330)
        let palavra = palavras !! n
        putStrLn "Tente adivinhar:"
        jogar palavra

obterChar :: IO Char
obterChar = do
            hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

jogar :: String -> IO ()
jogar palavra = do
                    putStr "? "
                    tentativa <- getLine
                    if (map toUpper tentativa) == palavra then
                        putStrLn "\nVOCE ACERTOU!\n"
                    else do
                            putStrLn (encontrar palavra tentativa)
                            jogar palavra

encontrar :: String -> String -> String
encontrar xs ys = [if x `elem` (map toUpper ys) then x else '_' | x <- xs]
