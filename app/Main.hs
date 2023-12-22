module Main where

import Control.Monad (foldM_)
import Data.Bifunctor
import Data.Char (digitToInt, intToDigit, isAlphaNum, isNumber, isSymbol, isUpper)
import Data.Foldable
import Data.List
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tuple (swap)
import Data.Type.Coercion (sym)
import Data.Void
import GHC.Plugins (DynFlags (backend), sep)
import GHC.Utils.Binary (Binary (put))
import GHC.Utils.Misc (split)
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer (symbol)
import Text.Megaparsec.Char

type Position = (Int, Int)

type MatrixNum = ([Position], Int)

type MatrixSymbol = (Position, Char)

data Cube = Red | Green | Blue
  deriving (Show, Eq, Ord)

-- type ReadS a = String -> [(a, String)]

data Game = Game {gameId :: Int, rounds :: [[(Int, Cube)]]}
  deriving (Show)

type Parser = Parsec Void String

pCubeOccurs :: Parser (Int, Cube)
pCubeOccurs = do
  _ <- space
  occur <- read <$> many digitChar
  _ <- space
  cube <-
    choice
      [ Red <$ string "red",
        Green <$ string "green",
        Blue <$ string "blue"
      ]
  return (occur, cube)

pRound :: Parser [(Int, Cube)]
pRound = sepEndBy pCubeOccurs (char ',')

pGame :: Parser Game
pGame = do
  _ <- string "Game "
  gameId <- read <$> many digitChar
  _ <- char ':'
  rounds <- sepEndBy pRound (char ';')
  return $ Game gameId rounds

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
allCombos (x : xs) = prefixes (length xs) ++ allCombos xs
  where
    prefixes :: Int -> [String]
    prefixes 0 = [[x]]
    prefixes n = (x : take n xs) : prefixes (n - 1)

trebuchet :: [String] -> Int
trebuchet =
  foldr
    ( \ln acc ->
        case ( do
                 firstN <- find isNumber ln
                 lastN <- find isNumber (reverse ln)
                 return $ read [firstN, lastN]
             ) of
          Nothing -> acc
          Just n -> acc + n
    )
    0

trebuchet' :: [String] -> Int
trebuchet' =
  foldr
    ( \ln acc ->
        case ( do
                 firstN <- find isDigit (allCombos ln)
                 lastN <- find isDigit (reverse $ allCombos ln)
                 return $ read [digitStringToChar firstN, digitStringToChar lastN]
             ) of
          Nothing -> acc
          Just n -> acc + n
    )
    0

isRoundPossible :: [(Int, Cube)] -> [(Int, Cube)] -> Bool
isRoundPossible config =
  foldr
    ( \cubeOccur acc ->
        acc && case find ((== snd cubeOccur) . snd) config of
          Just (configCubeOccurs, _) -> configCubeOccurs >= fst cubeOccur
          Nothing -> False
    )
    True

cubeConundrum :: [(Int, Cube)] -> [Game] -> Int
cubeConundrum config =
  sum . map gameId . filter (all (isRoundPossible config) . rounds)

roundToMap :: [(Int, Cube)] -> Map.Map Cube Int
roundToMap = Map.fromList . map swap

takeBiggerValue :: WhenMatched Maybe Cube Int Int Int
takeBiggerValue =
  zipWithAMatched
    ( \_ x y ->
        Just $ max x y
    )

minimumConfig :: Game -> [(Int, Cube)]
minimumConfig game =
  map swap $
    Map.toList $
      foldr
        ( \r acc ->
            fromMaybe
              acc
              ( mergeA
                  preserveMissing
                  preserveMissing
                  takeBiggerValue
                  (roundToMap r)
                  acc
              )
        )
        Map.empty
        (rounds game)

cubeConundrum' :: [Game] -> Int
cubeConundrum' =
  sum . map (product . map fst . minimumConfig)

getIndices :: Int -> Int -> (Int, Int)
getIndices offset numCols = (offset `div` numCols, offset `mod` numCols)

isSymbol' :: Char -> Bool
isSymbol' c = not (isAlphaNum c) && c /= '.' && c /= '\n'

pSymbol :: Parser Char
pSymbol = satisfy isSymbol'

pPeriod :: Parser Char
pPeriod = satisfy (== '.')

pWithOffset :: Parser a -> Parser (a, Int)
pWithOffset pa = do
  i <- getOffset
  a <- pa
  return (a, i)

pDigitsWithOffsets :: Parser [(Char, Int)]
pDigitsWithOffsets = some $ pWithOffset digitChar

pDigitsWithOffsets' :: Parser String
pDigitsWithOffsets' = takeWhile1P Nothing isNumber

pSymbolWithOffset :: Parser (Char, Int)
pSymbolWithOffset = pWithOffset pSymbol

pMatrixSymbol :: Int -> Parser MatrixSymbol
pMatrixSymbol numCols = (\(c, i) -> (getIndices i numCols, c)) <$> pSymbolWithOffset

pMatrixNum :: Int -> Parser MatrixNum
pMatrixNum numCols =
  (\(x, y) -> (x, read y)) . foldr (\(c, i) acc -> (getIndices i numCols : fst acc, c : snd acc)) ([], "") <$> pDigitsWithOffsets

pMatrixNumOrSymbol :: Int -> Parser (Either MatrixNum MatrixSymbol)
pMatrixNumOrSymbol numCols = Left <$> pMatrixNum numCols <|> Right <$> pMatrixSymbol numCols

pMatrixNumOrSymbolOrPeriod :: Int -> Parser (Either (Either MatrixNum MatrixSymbol) Char)
pMatrixNumOrSymbolOrPeriod numCols = Left <$> pMatrixNumOrSymbol numCols <|> Right <$> pPeriod

pSchematic :: Int -> Parser ([MatrixNum], [MatrixSymbol])
pSchematic numCols =
  foldr
    ( \e acc ->
        case e of
          Left e' ->
            case e' of
              Left mn -> first (mn :) acc
              Right ms -> second (ms :) acc
          Right _ -> acc
    )
    ([], [])
    <$> some (pMatrixNumOrSymbolOrPeriod numCols)

hasAdjacentSymbol :: Map.Map Position Char -> MatrixNum -> Bool
hasAdjacentSymbol symbolMap (positions, num) =
  any
    ( \(i, j) ->
        let hasSymbolTopLeft = Map.member (i - 1, j - 1) symbolMap
            hasSymbolTopMiddle = Map.member (i - 1, j) symbolMap
            hasSymbolTopRight = Map.member (i - 1, j + 1) symbolMap
            hasSymbolMiddleRight = Map.member (i, j + 1) symbolMap
            hasSymbolBottomRight = Map.member (i + 1, j + 1) symbolMap
            hasSymbolBottomMiddle = Map.member (i + 1, j) symbolMap
            hasSymbolBottomLeft = Map.member (i + 1, j - 1) symbolMap
            hasSymbolMiddleLeft = Map.member (i, j - 1) symbolMap
         in hasSymbolTopLeft
              || hasSymbolTopMiddle
              || hasSymbolTopRight
              || hasSymbolMiddleLeft
              || hasSymbolMiddleRight
              || hasSymbolBottomLeft
              || hasSymbolBottomMiddle
              || hasSymbolBottomRight
    )
    positions

--todo should actually be a map of position -> int! flatten the indicies!!
getAdjacentNumbers :: Map.Map Position Int -> MatrixSymbol -> [Int]
getAdjacentNumbers matrixNumMap ((i, j), _) =
  let surroundingIndices =
        [ (i - 1, j - 1),
          (i - 1, j),
          (i - 1, j + 1),
          (i, j + 1),
          (i + 1, j + 1),
          (i + 1, j),
          (i + 1, j - 1),
          (i, j - 1)
        ]
   in foldr
        (\pos acc ->
            case Map.lookup pos matrixNumMap of
              Nothing -> acc
              Just x -> x : acc 
        )
        []
        surroundingIndices

collectGears :: Map.Map Position Int -> [MatrixSymbol] -> [(Int, Int)]
collectGears matrixNumMap symbols =
  foldr
    ( \((i, j), c) acc ->
        if c == '*'
          then
            let adjNums = getAdjacentNumbers matrixNumMap ((i, j), c)
             in if length adjNums == 2
                  then (head adjNums, adjNums!!1) : acc
                  else acc
          else acc
    )
    []
    symbols

createNumPosMap :: [MatrixNum] -> Map.Map Position Int
createNumPosMap mNums =
  Map.fromList $
      foldr
      (\(positions, num) acc ->
        acc ++ map (\p -> (p, num)) positions
      )
      []
      mNums
gearRatios :: [MatrixNum] -> [MatrixSymbol] -> Int
gearRatios numbers symbols =
  let symbolMap = Map.fromList symbols
   in sum $ map snd $ filter (hasAdjacentSymbol symbolMap) numbers

gearRatios' :: [MatrixNum] -> [MatrixSymbol] -> Int
gearRatios' numbers symbols =
   sum $ map (uncurry (*)) $
       collectGears (createNumPosMap numbers) symbols

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
      let config = [(12, Red), (13, Green), (14, Blue)]
      let result = cubeConundrum config games
      writeFile "dist/day_2_1_output.txt" $ show result
      return result

day2p2 :: IO Int
day2p2 = do
  day2p1Input <- readFile "resources/day_2_1_input.txt"
  let gamesParsed = parse pGames "resources/day_2_1_input.txt" day2p1Input
  case gamesParsed of
    Left e -> error "error parsing"
    Right games -> do
      let result = cubeConundrum' games
      writeFile "dist/day_2_2_output.txt" $ show result
      return result

day3p1 :: IO Int
day3p1 = do
  day3p1Input <- readFile "resources/day_3_1_input.txt"
  let lns = map (++ ['.']) $ lines day3p1Input
  let numCols = length $ head lns
  let schematicParsed = parse (pSchematic numCols) "resources/day_3_1_input.txt" $ concat lns
  case schematicParsed of
    Left e -> error "error parsing"
    Right schematic -> do
      let result = uncurry gearRatios schematic
      writeFile "dist/day_3_1_output.txt" $ show result
      return result

day3p2 :: IO Int
day3p2 = do
  day3p1Input <- readFile "resources/day_3_1_input.txt"
  let lns = map (++ ['.']) $ lines day3p1Input
  let numCols = length $ head lns
  let schematicParsed = parse (pSchematic numCols) "resources/day_3_1_input.txt" $ concat lns
  case schematicParsed of
    Left e -> error "error parsing"
    Right schematic -> do
      let result = uncurry gearRatios' schematic
      writeFile "dist/day_3_2_output.txt" $ show result
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
  day2p2Result <- day2p2
  putStrLn $ "day 2 - part 2 - result: " ++ show day2p2Result
  day3p1Result <- day3p1
  putStrLn $ "day 3 - part 1 - result: " ++ show day3p1Result
  day3p2Result <- day3p2
  putStrLn $ "day 3 - part 2 - result: " ++ show day3p2Result
