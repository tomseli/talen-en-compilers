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

data Event = Event { dtstamp     :: DateTime
                   , uid         :: String
                   , dtstart     :: DateTime
                   , dtend       :: DateTime
                   , description :: Maybe String
                   , summary     :: Maybe String
                   , location    :: Maybe String }
  deriving (Eq, Ord, Show)

newtype Text = Text String deriving (Show, Eq, Ord)

-- Exercise 7
data Token = TBeginCalendar
           | TEndCalendar
           | TBeginEvent
           | TEndEvent
           | TVersion
           | TProdID        Text
           | TUID           Text
           | TDTStamp       Text
           | TDTStart       Text
           | TDTEnd         Text
           | TDTDescription Text
           | TDTSummary     Text
           | TDTLocation    Text
    deriving (Eq, Ord, Show)

fromText :: Text -> String 
fromText (Text s) = s

getTokenValue :: Token -> String
getTokenValue (TProdID t)        = fromText t 
getTokenValue (TUID t)           = fromText t 
getTokenValue (TDTStamp t)       = fromText t 
getTokenValue (TDTStart t)       = fromText t 
getTokenValue (TDTEnd t)         = fromText t 
getTokenValue (TDTDescription t) = fromText t 
getTokenValue (TDTSummary t)     = fromText t 
getTokenValue (TDTLocation t)    = fromText t 
getTokenValue _ = error "Uh oh"


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

parseText :: String -> Parser Char Text
parseText s = Text <$> (token s *> collectText)
  where
    collectText = many (satisfy (/= '\r')) >>= \c -> token "\r\n" >>= \s -> look >>= \r -> f c r s
    f c r s = case r of
        (' ' : _) -> anySymbol >> collectText >>= \m -> return (c ++ s ++ " " ++ m)
        _         -> return c

-- "\r\n " -> False
-- isTextEnding :: -> Bool


-- text may have new lines, incorperate that
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
       <|> TDTDescription <$> parseText "DESCRIPTION:" 
       <|> TDTSummary     <$> parseText "SUMMARY:"     
       <|> TDTLocation    <$> parseText "LOCATION:"    

lexCalendar :: Parser Char [Token]
lexCalendar = greedy tokenize

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> parseProperties <*> parseEvents 

parseProperties :: Parser Token [CalProperties]
parseProperties = undefined 

parseProperty :: Parser Token CalProperties
parseProperty = undefined -- CalProperties <$> parseToken <*> parseVersion

parseProdID :: Parser Token String 
parseProdID = anySymbol >>= \c -> if p c then return $ getTokenValue c else empty
  where p (TProdID _) = True
        p _           = False 

parseVersion :: Parser Token Bool 
parseVersion = undefined

parseEvents :: Parser Token [Event]
parseEvents = undefined 

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined