module Components.Pomodoro.PomodoroLog where

import Date exposing (Date, toTime, fromTime)
import List exposing(length, append)
import Maybe exposing (withDefault, map)
import Time exposing (..)

import Common.EventUtils exposing (onChange, onInput, onSubmitPreventDefault)
import Common.TimeUtils exposing (extractDatesUniqueToMonthDay,  haveSameDayAndMonth, getMonthDayString)

-- MODEL

type alias PomodoroLog = {
    timeStarted: Time.Time,
    timeFinished: Time.Time,
    finished: Bool
}

type alias Log = {
    pomodoros: List PomodoroLog
}

initialLog : Log
initialLog = {
                pomodoros = []
            }



getPomodorosNearDate : List PomodoroLog -> Date -> List PomodoroLog
getPomodorosNearDate log date = 
                           log 
                           |> List.filter ( (haveSameDayAndMonth date) << fromTime << .timeStarted ) 

extractLogStampsGroupedByMonthDay : List PomodoroLog -> List (Date, (List PomodoroLog) )
extractLogStampsGroupedByMonthDay log = 
                    let
                        dates = extractDatesUniqueToMonthDay << List.map (fromTime << .timeStarted) <| log
                    in
                        List.map ( \date -> (date, getPomodorosNearDate log date)) dates
                        



-- UPDATE

type LoggerAction = AppendPomodoroLog PomodoroLog
                   |AppendGTDLog

update: LoggerAction -> Log -> Log
update action log = 
            case action of 
                AppendPomodoroLog (pomoLog) ->
                    { log |
                        pomodoros <- (pomoLog :: log.pomodoros)
                    }
                AppendGTDLog -> log



