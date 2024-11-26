module DateTime where

import Data.Time.Calendar (fromGregorianValid)
import Data.Time.LocalTime (makeTimeOfDayValid)

import ParseLib
import Control.Applicative
import Control.Monad

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
  deriving (Eq, Ord, Show)

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

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseTimeUTC

parseDate :: Parser Char Date
parseDate = Date <$> parseGenericDigits Year 4 <*> parseGenericDigits Month 2 <*> parseGenericDigits Day 2

-- this is a very common pattern, type classable? 
-- would allow for parseGenericDigits 4 instead of parseGenericDigits Year 4
parseGenericDigits :: (Int -> a) -> Int -> Parser Char a
parseGenericDigits a n = readStringInt a <$> parseDigits n

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

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

-- tries to parse 'Z', if it fails return False
-- to avoid having two succesful results, the biased left hand operator is used
parseTimeUTC :: Parser Char Bool
parseTimeUTC = True <$ symbol 'Z' <<|> pure False

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = case parse p s of
  []         -> Nothing
  (a, _):_  -> Just a

-- Exercise 3
testDateTime :: DateTime
testDateTime = DateTime { date = Date { year = Year {runYear =1997}
                                      , month = Month {runMonth = 6}
                                      , day = Day {runDay = 10}
                                      }
                        , time = Time { hour = Hour {runHour = 17}
                                      , minute = Minute {runMinute = 23}
                                      , second = Second {runSecond = 45}
                                      }
                        , utc = True
                        }

printDateTime :: DateTime -> String
printDateTime dt = printDate (date dt)
                ++ "T"
                ++ printTime (time dt)
                ++ printZulu (utc dt)

printDate :: Date -> String
printDate d = show4 (runYear (year d))
           ++ show2 (runMonth (month d))
           ++ show2 (runDay (day d))

printTime :: Time -> String
printTime t = show2 (runHour (hour t))
           ++ show2 (runMinute (minute t))
           ++ show2 (runSecond (second t))

show2 :: Show a => a -> String
show2 x = if length s < 2 then '0' : s else s
  where s = show x

show4 :: Show a => a -> String
show4 x = if length s < 4 then '0' : s else s
  where s = show x

printZulu :: Bool -> String
printZulu True  = "Z"
printZulu False = ""

-- Exercise 4
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime dt = checkDate (date dt) && checkTime (time dt) 

checkDate :: Date -> Bool
checkDate d = case fromGregorianValid 
  (toInteger (runYear (year d))) 
  (runMonth (month d)) 
  (runDay (day d)) of
    Just _ -> True 
    Nothing -> False

checkTime :: Time -> Bool 
checkTime t = case makeTimeOfDayValid 
  (runHour $ hour t) 
  (runMinute $ minute t) 
  (fromIntegral $ runSecond $ second t) of 
    Just _ -> True 
    Nothing -> False