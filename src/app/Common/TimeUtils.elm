module Common.TimeUtils where

import Time exposing (..)
import String exposing (padLeft)

formatTimeString: Time.Time -> String
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
