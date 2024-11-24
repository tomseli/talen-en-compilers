module Calendar where

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
newtype Token = Token [String]
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
