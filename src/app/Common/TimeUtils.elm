module Common.TimeUtils where

import Time exposing (..)
import Date exposing (..)
import String exposing (padLeft)
import List.Extra exposing (groupBy)
import Maybe


getMonthDayString : Date -> (String, String)
getMonthDayString date = ( (toString << month <| date) , (toString <| day <| date) )


haveSameDayAndMonth : Date -> Date -> Bool
haveSameDayAndMonth date1 date2 = 
                    (month date1 == month date2) && (day date1 == day date2)


extractDatesUniqueToMonthDay : List Date -> List Date
extractDatesUniqueToMonthDay dates =
                    dates
                    |> groupBy (haveSameDayAndMonth)
                    |> List.map (Maybe.withDefault (fromTime 0) << List.head)

getMonthString : Date -> String
getMonthString d = 
        case month d of
            Jan -> "Jan"
            Feb -> "Feb"
            Mar -> "Mar"
            Apr -> "Apr"
            May -> "May"
            Jun -> "Jun"
            Jul -> "Jul"
            Aug -> "Aug"
            Sep -> "Sep"
            Oct -> "Oct"
            Nov -> "Nov"
            Dec -> "Dec"

formatTimeString : Time.Time -> String
formatTimeString time = 
             let 
                
                seconds =   time |> Time.inSeconds |> truncate 
                minutes =   seconds // 60

                secondsMod = seconds % 60
                minutesMod = minutes

                secondsString = secondsMod |> toString |> padLeft 2 '0'
                minutesString = minutesMod |> toString |> padLeft 2 '0'

             in 
                minutesString ++ ":" ++ secondsString
