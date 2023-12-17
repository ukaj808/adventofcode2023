module Main where
import Data.Char (isNumber, digitToInt, intToDigit, isUpper)
import Data.Foldable
import GHC.Utils.Misc (split)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import GHC.Plugins (sep)
import System.Directory
import Data.List

data Cube = Red | Green | Blue
    deriving (Show, Eq, Ord)
-- type ReadS a = String -> [(a, String)]

data Game = Game { gameId :: Int, rounds :: [[(Int, Cube)]] }
    deriving (Show)

type Parser = Parsec Void String

pCubeOccurs :: Parser (Int, Cube)
pCubeOccurs = do
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

pHandfulOfCubes :: Parser [(Int, Cube)]
pHandfulOfCubes = sepEndBy pCubeOccurs (char ',')

pGame :: Parser Game
pGame = do
    _ <- string "Game "
    gameId <- read <$> many digitChar
    _ <- char ':'
    handfuls <- sepEndBy pHandfulOfCubes (char ';')
    return $ Game gameId handfuls

pGames :: Parser [Game]
pGames = sepEndBy pGame eol


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


isHandfulPossible :: [(Int, Cube)] -> [(Int, Cube)] -> Bool
isHandfulPossible config = foldr
    (\cubeOccur acc ->
        acc && case find ((== snd cubeOccur) . snd) config of
            Just (configCubeOccurs, _) -> configCubeOccurs >= fst cubeOccur
            Nothing -> False
    ) True

cubeConundrum :: [(Int, Cube)] -> [Game] -> Int
cubeConundrum config =
    sum . map gameId . filter (all (isHandfulPossible config) . rounds)

cubeConundrum' :: [(Int, Cube)] -> [Game] -> Int
cubeConundrum' config = foldr
    (\game acc ->
        case game of
            Game id handfuls ->
                if all (isHandfulPossible config) handfuls
                then acc + id
                else acc
    ) 0

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
    let gamesParsed = parse pGames "resources/day_2_1_input.txt" day2p1Input
    case gamesParsed of
        Left e -> error "error parsing"
        Right games -> do
            let config = [(12, Red),(13, Green),(14, Blue)]
            let result = cubeConundrum config games
            writeFile "dist/day_2_1_output.txt" $ show result
            return result

main :: IO ()
main = do
    createDirectoryIfMissing True "dist"
    day1p1Result <- day1p1
    putStrLn $ "day 1 - part 1 - result: " ++ show day1p1Result
    day1p2Result <- day1p2
    putStrLn $ "day 1 - part 2 - result: " ++ show day1p2Result
    day2p1Result <- day2p1
    putStrLn $ "day 2 - part 1 - result: " ++ show day2p1Result
