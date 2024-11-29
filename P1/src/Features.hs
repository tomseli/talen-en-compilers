module Features where

import DateTime
import Calendar

import Data.Time


-- Exercise 9
countEvents :: Calendar -> Int
countEvents cal = length $ event cal

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt cal = [e | e <- event cal 
                            , dtstart e >= dt
                            , dtend e < dt ]

checkOverlapping :: Calendar -> Bool
checkOverlapping cal = or [ isOverlapping e1 e2 | 
                            e1 <- event cal, e2 <- event cal ]

isOverlapping :: Event -> Event -> Bool
isOverlapping e1 e2 = dtstart e2 < dtend e1 && dtstart e2 < dtstart e1

timeSpent :: String -> Calendar -> Int
timeSpent s cal = foldr (\x a -> countDiffMinutes x + a) 0 [e | e <- event cal, summary e == Just s]

countDiffMinutes :: Event -> Int
countDiffMinutes e = (floor . realToFrac) 
                   $ abs 
                   $ diffUTCTime (toUTCTime $ dtend e) (toUTCTime $ dtstart e) / 60

toUTCTime :: DateTime -> UTCTime
toUTCTime (DateTime (Date (Year y) (Month m) (Day d)) (Time (Hour h) (Minute min) (Second s)) _) =
    let day = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
        timeOfDay = secondsToDiffTime (toInteger h * 3600 + toInteger min * 60 + toInteger s)
    in UTCTime day timeOfDay