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
\ Hello world\r\n\
\END:VEVENT\r\n\
\END:VCALENDAR\r\n"

dtFailure :: DateTime
dtFailure = DateTime { date = Date { year = Year {runYear = 0000}
                                    , month = Month {runMonth = 0}
                                    , day = Day {runDay = 0}
                                    }
                      , time = Time { hour = Hour {runHour = 0}
                                    , minute = Minute {runMinute = 0}
                                    , second = Second {runSecond = 0}
                                    }
                      , utc = True
                      }

tokenizeBeginCalendar :: Parser Char Token
tokenizeBeginCalendar = TBeginCalendar <$ token "BEGIN:VCALENDAR\r\n"

tokenizeEndCalendar:: Parser Char Token
tokenizeEndCalendar = TEndCalendar <$ token "END:VCALENDAR\r\n"

tokenizeBeginEvent :: Parser Char Token
tokenizeBeginEvent = TBeginEvent <$ token "BEGIN:VEVENT\r\n"

tokenizeEndEvent :: Parser Char Token
tokenizeEndEvent = TEndEvent <$ token "END:VEVENT\r\n"

tokenizeVersion :: Parser Char Token
tokenizeVersion = TVersion <$ token "VERSION:2.0\r\n"

parseText :: String -> Parser Char String
parseText s = token s *> collectText
  where
    collectText = many (satisfy (/= '\r')) >>= \c -> token "\r\n" >>= \s -> look >>= \r -> f c r s
    f c r s = case r of
        (' ' : _) -> anySymbol >> collectText >>= \m -> return (c ++ s ++ " " ++ m)
        _         -> return c

tokenize :: Parser Char Token
tokenize = tokenizeBeginCalendar
       <|> tokenizeEndCalendar
       <|> tokenizeBeginEvent
       <|> tokenizeEndEvent
       <|> tokenizeVersion
       <|> TProdID        <$> parseText "PRODID:"
       <|> TUID           <$> parseText "UID:"
       <|> TDTStamp       <$> parseText "DTSTAMP:"
       <|> TDTStart       <$> parseText "DTSTART:"
       <|> TDTEnd         <$> parseText "DTEND:"
       <|> TDescription   <$> parseText "DESCRIPTION:"
       <|> TSummary       <$> parseText "SUMMARY:"
       <|> TLocation      <$> parseText "LOCATION:"

lexCalendar :: Parser Char [Token]
lexCalendar = greedy tokenize

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> parseProperties <*> parseEvents <* symbol TEndCalendar

parseProperties :: Parser Token [CalProperties]
parseProperties = greedy parseProperty

-- this function purposefully crashes when either version or prodid is not correct
-- list comprehension is used as a kind of filter
parseProperty :: Parser Token CalProperties
parseProperty = (\_ props -> buildProps props) <$> symbol TBeginCalendar <*> greedy (satisfy (/= TBeginEvent))
  where
    buildProps :: [Token] -> CalProperties
    buildProps tokens = let version = once [True | TVersion    <- tokens]
                            prodid  = once [x    | (TProdID x) <- tokens]
                        in CalProperties (fromJust prodid) (fromJust version)

parseEvents :: Parser Token [Event]
parseEvents = greedy parseEvent

-- this function purposefully crashes when either version or prodid is not correct
-- list comprehension is used as a kind of filter
parseEvent :: Parser Token Event
parseEvent = buildEvent <$> pack (symbol TBeginEvent) (greedy $ satisfy (/= TEndEvent)) (symbol TEndEvent)
  where
    buildEvent :: [Token] -> Event
    buildEvent tokens = let uid   = once [x | (TUID x) <- tokens]
                            stamp = once [x | (TDTStamp x) <- tokens]
                            start = once [x | (TDTStart x) <- tokens]
                            end   = once [x | (TDTEnd x) <- tokens]
                            des   = once [x | (TDescription x) <- tokens]
                            sum   = once [x | (TSummary x) <- tokens]
                            loc   = once [x | (TLocation x) <- tokens]
                         in Event (convertDT $ fromJust stamp)
                                  (fromJust uid)
                                  (convertDT $ fromJust start)
                                  (convertDT $ fromJust end)
                                  des
                                  sum
                                  loc

    convertDT :: String -> DateTime
    convertDT s = fromMaybe dtFailure $ run parseDateTime s

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

printProduction :: String -> String -> String 
printProduction k v = k ++ v ++ "\r\n"

printOptionalProduction :: String -> Maybe String -> String
printOptionalProduction k (Just v) = k ++ v ++ "\r\n"
printOptionalProduction k Nothing = ""

printCalendarHeader :: String
printCalendarHeader = "BEGIN:VCALENDAR\r\n"

printCalendarFooter :: String
printCalendarFooter = "END:VCALENDAR\r\n"

printEventHeader :: String
printEventHeader = "BEGIN:VEVENT\r\n"

printEventFooter :: String
printEventFooter = "END:VEVENT\r\n"