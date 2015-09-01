module Components.Pomodoro.PomodoroLog where

import Time exposing (..)


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



