module DateTime where

import ParseLib
import Control.Applicative
import Control.Monad

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)

-- Testing
data S = Minus D S | SingleDigit D deriving (Show)
data D = Zero | One deriving (Show)

parseD :: Parser Char D
parseD = Zero <$ symbol '0' 
  <|> One <$ symbol '1'

parseS :: Parser Char S
parseS = (\d _ s -> Minus d s) <$> parseD <*> symbol '-' <*> parseS 
  <|> SingleDigit <$> parseD

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = undefined

parseDate :: Parser Char Date
parseDate = undefined  

parseTime :: Parser Char Time 
parseTime = parseTime' <$> parseDigits 6

parseTime' :: String -> Time
parseTime' s = Time
  { hour   = undefined 
  , minute = undefined
  , second = undefined
  }

parseHour :: Parser Char Hour
parseHour = readStringInt Hour <$> parseTwoDigits

parseMinute :: Parser Char Minute
parseMinute = readStringInt Minute <$> parseTwoDigits 

parseSecond :: Parser Char Second 
parseSecond = readStringInt Second <$> parseTwoDigits

parseTwoDigits :: Parser Char [Char]
parseTwoDigits = replicateM 2 digit

parseDigits :: Int -> Parser Char [Char]
parseDigits n = replicateM n digit

readStringInt ::  (Int -> a) -> String -> a
readStringInt constructor s = constructor $ read s 


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
