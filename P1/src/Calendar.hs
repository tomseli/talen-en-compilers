module Calendar where

import Control.Applicative
import Control.Monad
import Data.Maybe

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

data Event = Event { dtstamp     :: DateTime
                   , uid         :: String
                   , dtstart     :: DateTime
                   , dtend       :: DateTime
                   , description :: Maybe String
                   , summary     :: Maybe String
                   , location    :: Maybe String }
  deriving (Eq, Ord, Show)

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
           | TDescription String
           | TSummary     String
           | TLocation    String
           deriving (Eq, Ord, Show)

-- | TOKENIZING | -- 

-- tokenize any literal text to a token
tokenizeLiteral :: Token -> String -> Parser Char Token
tokenizeLiteral t s = t <$ token s

-- this mess of a function parses untill \r\n followed by a non-space
-- maybe this can be done more pretty?
tokenizeText :: String -> Parser Char String
tokenizeText s = token s *> collectText
  where
    collectText = many (satisfy (/= '\r')) >>= \c -> token "\r\n" >> look >>= \r -> f c r 
    f c r = case r of
        (' ' : _) -> anySymbol >> collectText >>= \m -> return (c ++ m)
        _         -> return c

tokenize :: Parser Char Token
tokenize = tokenizeLiteral  TBeginCalendar "BEGIN:VCALENDAR\r\n"
       <|> tokenizeLiteral  TEndCalendar   "END:VCALENDAR\r\n"
       <|> tokenizeLiteral  TBeginEvent    "BEGIN:VEVENT\r\n"
       <|> tokenizeLiteral  TEndEvent      "END:VEVENT\r\n" 
       <|> tokenizeLiteral  TVersion       "VERSION:2.0\r\n"
       <|> TProdID      <$> tokenizeText   "PRODID:"
       <|> TUID         <$> tokenizeText   "UID:"
       <|> TDTStamp     <$> tokenizeText   "DTSTAMP:"
       <|> TDTStart     <$> tokenizeText   "DTSTART:"
       <|> TDTEnd       <$> tokenizeText   "DTEND:"
       <|> TDescription <$> tokenizeText   "DESCRIPTION:"
       <|> TSummary     <$> tokenizeText   "SUMMARY:"
       <|> TLocation    <$> tokenizeText   "LOCATION:"

lexCalendar :: Parser Char [Token]
lexCalendar = greedy tokenize

-- | PARSING | --

-- pack doesn't quite work here, because we want to run two parsers between delimeters
parseCalendar :: Parser Token Calendar
parseCalendar = (\_ a b -> Calendar a b) <$> symbol TBeginCalendar 
                                         <*> parseProperties 
                                         <*> parseEvents 
                                         <*  symbol TEndCalendar

parseProperties :: Parser Token [CalProperties]
parseProperties = greedy parseProperty

-- this function purposefully crashes when either version or prodid is missing
-- list comprehension is used as a kind of filter for constructors
parseProperty :: Parser Token CalProperties
parseProperty = buildProps <$> some (satisfy (/= TBeginEvent))
  where
    buildProps :: [Token] -> CalProperties
    buildProps tokens = let version = once [True | TVersion    <- tokens]
                            prodid  = once [x    | (TProdID x) <- tokens]
                        in CalProperties (fromJust prodid) (fromJust version)

parseEvents :: Parser Token [Event]
parseEvents = greedy parseEvent

-- this function purposefully crashes when a required token is missing
-- list comprehension is used as a kind of filter for constructors
parseEvent :: Parser Token Event
parseEvent = buildEvent <$> pack (symbol TBeginEvent) (greedy1 $ satisfy (/= TEndEvent)) (symbol TEndEvent)
  where
    buildEvent :: [Token] -> Event
    buildEvent tokens = let uid   = once [x | (TUID x)         <- tokens]
                            stamp = once [x | (TDTStamp x)     <- tokens]
                            start = once [x | (TDTStart x)     <- tokens]
                            end   = once [x | (TDTEnd x)       <- tokens]
                            des   = once [x | (TDescription x) <- tokens]
                            sum   = once [x | (TSummary x)     <- tokens]
                            loc   = once [x | (TLocation x)    <- tokens]
                         in Event (convertDT $ fromJust stamp)
                                  (fromJust uid)
                                  (convertDT $ fromJust start)
                                  (convertDT $ fromJust end)
                                  des sum loc

    convertDT :: String -> DateTime
    convertDT s = fromJust $ run parseDateTime s

-- check if list is a singleton
once :: [a] -> Maybe a
once [x] = Just x
once _   = Nothing

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar cal = printCalendarHeader
                 ++ printProperties (calprop cal)
                 ++ printEvents (event cal)
                 ++ printCalendarFooter 

printProperties :: [CalProperties] -> String
printProperties = concatMap printProperty

printProperty :: CalProperties -> String
printProperty prop = printProduction "PRODID:" (productid prop)
                  ++ printProduction "VERSION:" "2.0"

printEvents :: [Event] -> String
printEvents = concatMap printEvent

printEvent :: Event -> String
printEvent event = printEventHeader
                ++ printProduction "DTSTAMP:" (printDateTime $ dtstamp event) 
                ++ printProduction "UID:"     (uid event)
                ++ printProduction "DTSTART:" (printDateTime $ dtstart event)
                ++ printProduction "DTEND:"   (printDateTime $ dtend   event)
                ++ printOptionalProduction "DESCRIPTION:" (description event)
                ++ printOptionalProduction "SUMMARY:"     (summary     event)
                ++ printOptionalProduction "LOCATION:"    (location    event)
                ++ printEventFooter

-- it is unclear to me what exactly counts towards the 42 character limit
-- I've counted the header, but discounted the "\r\n" sequence
printProduction :: String -> String -> String 
printProduction _ [] = "" -- fixes a specific bug where the line is exactly 42 chars long
printProduction k v | length (k ++ v) < 42 = k ++ v ++ "\r\n"
                    | otherwise            = take 42 (k ++ v) ++ "\r\n" ++ printProduction " " (drop 42 (k ++ v))  

printOptionalProduction :: String -> Maybe String -> String
printOptionalProduction k (Just v) = printProduction k v
printOptionalProduction k Nothing = ""

printCalendarHeader :: String
printCalendarHeader = "BEGIN:VCALENDAR\r\n"

printCalendarFooter :: String
printCalendarFooter = "END:VCALENDAR\r\n"

printEventHeader :: String
printEventHeader = "BEGIN:VEVENT\r\n"

printEventFooter :: String
printEventFooter = "END:VEVENT\r\n"