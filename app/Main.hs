module Main where
import Data.Char (isNumber, digitToInt, intToDigit, isUpper)
import Data.Foldable
import GHC.Utils.Misc (split)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import GHC.Plugins (sep)

data Cube = Red | Green | Blue
    deriving (Show, Eq)
-- type ReadS a = String -> [(a, String)]

newtype HandfulOfCubes = HandFulOfCubes [(Int, Cube)]
 deriving (Show)

data Game = Game Int [HandfulOfCubes]
    deriving (Show)

newtype CubeGame = CubeGame [Game]
 deriving (Show)

type Parser = Parsec Void String

parseCubeOccurs :: Parser (Int, Cube)
parseCubeOccurs = do
    _ <- space
    occur <- read <$> many digitChar
    _  <- space
    cube <- choice
        [
            Red <$ string "red"
        ,   Green <$ string "green"
        ,   Blue  <$ string "blue"
        ]
    return (occur, cube)

pHandfulOfCubes :: Parser HandfulOfCubes
pHandfulOfCubes = do
    HandFulOfCubes <$>
        sepEndBy parseCubeOccurs (char ',')

parseGame :: Parser Game
parseGame = do
    _ <- string "Game "
    gameId <- read <$> many digitChar
    _ <- char ':'
    handfuls <- sepEndBy pHandfulOfCubes (char ';')
    return $ Game gameId handfuls

pCubeGame :: Parser CubeGame
pCubeGame = do
    CubeGame <$>
        sepEndBy parseGame eol
  

isDigit :: String -> Bool
isDigit "one" = True
isDigit "two" = True
isDigit "three" = True
isDigit "four" = True
isDigit "five" = True
isDigit "six" = True
isDigit "seven" = True
isDigit "eight" = True
isDigit "nine" = True
isDigit "zero" = True
isDigit "1" = True
isDigit "2" = True
isDigit "3" = True
isDigit "4" = True
isDigit "5" = True
isDigit "6" = True
isDigit "7" = True
isDigit "8" = True
isDigit "9" = True
isDigit "0" = True
isDigit _ = False

digitStringToInt :: String -> Int
digitStringToInt "one" = 1
digitStringToInt "two" = 2
digitStringToInt "three" = 3
digitStringToInt "four" = 4
digitStringToInt "five" = 5
digitStringToInt "six" = 6
digitStringToInt "seven" = 7
digitStringToInt "eight" = 8
digitStringToInt "nine" = 9
digitStringToInt "zero" = 0
digitStringToInt "1" = 1
digitStringToInt "2" = 2
digitStringToInt "3" = 3
digitStringToInt "4" = 4
digitStringToInt "5" = 5
digitStringToInt "6" = 6
digitStringToInt "7" = 7
digitStringToInt "8" = 8
digitStringToInt "9" = 9
digitStringToInt "0" = 10
digitStringToInt _ = undefined


digitStringToChar :: String -> Char
digitStringToChar = intToDigit . digitStringToInt

allCombos :: String -> [String]
allCombos [] = []
allCombos (x:xs) = prefixes (length xs) ++ allCombos xs
    where
        prefixes :: Int -> [String]
        prefixes 0 = [[x]]
        prefixes n = (x : take n xs) : prefixes (n - 1)

trebuchet :: [String] -> Int
trebuchet =
    foldr
    (\ln acc ->
        case
            (do
                firstN <- find isNumber ln
                lastN  <- find isNumber (reverse ln)
                return $ read [firstN, lastN]
            )
        of
            Nothing -> acc
            Just n  -> acc + n
    ) 0


trebuchet' :: [String] -> Int
trebuchet' =
    foldr
    (\ln acc ->
        case
            (do
                firstN <- find isDigit (allCombos ln)
                lastN  <- find isDigit (reverse $ allCombos ln)
                return $ read [digitStringToChar firstN, digitStringToChar lastN]
            )
        of
            Nothing -> acc
            Just n  -> acc + n
    ) 0

cubeConundrum :: CubeGame -> Int
cubeConundrum cubeGame = 0

day1p1 :: IO Int
day1p1 = do
    day1p1Input <- readFile "resources/day_1_1_input.txt"
    let result = trebuchet $ lines day1p1Input
    writeFile "dist/day_1_1_output.txt" $ show result
    return result

day1p2 :: IO Int
day1p2 = do
    day1p2Input <- readFile "resources/day_1_2_input.txt"
    let result = trebuchet' $ lines day1p2Input
    writeFile "dist/day_1_2_output.txt" $ show result
    return result

day2p1 :: IO Int
day2p1 = do
    day2p1Input <- readFile "resources/day_2_1_input.txt"
    let cubeGameParsed = parse pCubeGame "resources/day_2_1_input.txt" day2p1Input
    case cubeGameParsed of
        Left e -> error "error parsing"
        Right cubeGame -> do
            print cubeGame
            let result = cubeConundrum cubeGame
            writeFile "dist/day_2_1_output.txt" $ show result
            return result

main :: IO ()
main = do
    day1p1Result <- day1p1
    putStrLn $ "day 1 - part 1 - result: " ++ show day1p1Result
    day1p2Result <- day1p2
    putStrLn $ "day 1 - part 2 - result: " ++ show day1p2Result
    day2p1Result <- day2p1
    putStrLn $ "day 2 - part 1 - result: " ++ show day2p1Result
