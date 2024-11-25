module Calendar where

import Control.Applicative
import Control.Monad

import           DateTime
import           ParseLib


-- Exercise 6
data Calendar = Calendar { calprop :: [CalProperties]
                         , event   :: [Event] }
  deriving (Eq, Ord, Show)

-- the contents of version don't matter, it just needs to be there
data CalProperties = CalProperties { productid :: String
                                   , version   :: Bool }
    deriving (Eq, Ord, Show)

data Event = Event { dtstamp     :: Maybe DateTime
                   , uid         :: Maybe String
                   , dtstart     :: Maybe DateTime
                   , dtend       :: Maybe DateTime
                   , description :: Maybe String
                   , summary     :: Maybe String
                   , location    :: Maybe String }
  deriving (Eq, Ord, Show)

-- keep these in mind, i guess
type UID         = String
type Description = String
type Summary     = String
type Location    = String

-- Exercise 7
data Token = TBeginCalendar
           | TEndCalendar
           | TBeginEvent
           | TEndEvent
           | TVersion
           | TProdID        String
           | TUID           String
           | TDTStamp       String
           | TDTStart       String
           | TDTEnd         String
           | TDTDescription String
           | TDTSummary     String
           | TDTLocation    String
    deriving (Eq, Ord, Show)

testString :: String
testString = "BEGIN:VCALENDAR\r\n\
\VERSION:2.0\r\n\
\PRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\n\
\BEGIN:VEVENT\r\n\
\UID:19970610T172345Z-AF23B2@example.com\r\n\
\DTSTAMP:19970610T172345Z\r\n\
\DTSTART:19970714T170000Z\r\n\
\DTEND:19970715T040000Z\r\n\
\SUMMARY:Bastille Day Party\r\n\
\END:VEVENT\r\n\
\END:VCALENDAR\r\n"

tokenizeBeginCalendar :: Parser Char Token
tokenizeBeginCalendar = TBeginCalendar <$ token "BEGIN:VCALENDAR\r\n"

tokenizeEndCalendar:: Parser Char Token
tokenizeEndCalendar = TEndCalendar <$ token "END:VCALENDAR\r\n"

tokenizeBeginEvent :: Parser Char Token 
tokenizeBeginEvent = TBeginEvent <$ token "BEGIN:VEVENT\r\n"

tokenizeEndEvent :: Parser Char Token 
tokenizeEndEvent = TBeginEvent <$ token "END:VEVENT\r\n"

tokenizeVersion :: Parser Char Token 
tokenizeVersion = TVersion <$ token "VERSION:2.0\r\n"

-- TODO, improve failure condition to include both \r\n

tokenizeGenericString :: String -> (String -> Token) -> Parser Char Token 
tokenizeGenericString s c = (\_ s _ -> c s) <$> token s <*> many (satisfy (/= '\r')) <*> token "\r\n" 

tokenizeProdID :: Parser Char Token 
tokenizeProdID = (\_ s _ -> TProdID s) <$> token "PRODID:" <*> many (satisfy (/= '\r')) <*> token "\r\n" 

-- text may have new lines, incorperate that
tokenize :: Parser Char Token 
tokenize = tokenizeBeginCalendar 
       <|> tokenizeEndCalendar 
       <|> tokenizeBeginEvent 
       <|> tokenizeEndEvent 
       <|> tokenizeVersion
       <|> tokenizeGenericString "PRODID:"      TProdID
       <|> tokenizeGenericString "UID:"         TUID
       <|> tokenizeGenericString "DTSTAMP:"     TDTStamp
       <|> tokenizeGenericString "DTSTART:"     TDTStart
       <|> tokenizeGenericString "DTEND:"       TDTEnd
       <|> tokenizeGenericString "DESCRIPTION:" TDTDescription
       <|> tokenizeGenericString "SUMMARY:"     TDTSummary 
       <|> tokenizeGenericString "LOCATION:"    TDTLocation

lexCalendar :: Parser Char [Token]
lexCalendar = many tokenize

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined