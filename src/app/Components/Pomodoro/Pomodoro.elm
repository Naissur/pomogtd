module Components.Pomodoro.Pomodoro where

import Html exposing (Html, p, h2, div, button, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Signal exposing (..)
import Time exposing (..)
import Maybe exposing (andThen)

import Common.TimeUtils as TimeUtils

import Components.Pomodoro.PomodoroLog as PomodoroLog



-- MODEL

type Phase = Working
            |SmallBreak

serealizePhase : Phase -> String
serealizePhase phase = 
        case phase of
            Working ->      "Working"
            SmallBreak ->   "SmallBreak"


deserealizePhase : String -> Maybe Phase
deserealizePhase phase = 
        case phase of
            "Working" ->     Just Working
            "SmallBreak" ->  Just SmallBreak
            otherwise ->     Nothing


type alias Model = {
    started: Bool,
    paused: Bool,
    phase: Phase,
    timeStarted: Time.Time,
    timeEnding: Time.Time,
    timeLeft: Time.Time,

    log: PomodoroLog.Log
}

type alias SerealizedModel = {
    started: Bool,
    paused: Bool,
    phase: String,
    timeStarted: Time.Time,
    timeEnding: Time.Time,
    timeLeft: Time.Time,

    log: PomodoroLog.Log
}


serealizeModel : Model -> SerealizedModel
serealizeModel model = 
        {
            started = model.started,
            paused = model.paused,
            phase = (serealizePhase model.phase),
            timeStarted = model.timeStarted,
            timeEnding = model.timeEnding,
            timeLeft = model.timeLeft,

            log = model.log
        }

deSerealizeModel : SerealizedModel -> Maybe Model
deSerealizeModel model = 
        (deserealizePhase model.phase) `andThen` 

        \deserealizedPhase ->
          Just  {
                    started = model.started,
                    paused = model.paused,
                    phase = deserealizedPhase,
                    timeStarted = model.timeStarted,
                    timeLeft = model.timeLeft,
                    timeEnding = model.timeEnding,

                    log = model.log
                }


initialModel : Model
initialModel = {
        started = False,
        paused = False,
        phase = Working,
        timeStarted = 0,
        timeEnding = 0,
        timeLeft = workTime,

        log = PomodoroLog.initialLog
    }



workTime: Time.Time
workTime = Time.minute * 25
--workTime = Time.second * 3

smallBreakTime: Time.Time
smallBreakTime  = Time.minute * 5
--smallBreakTime  = Time.second * 4

largeBreakTime: Time.Time
--largeBreakTime  = Time.minute * 15
largeBreakTime  = Time.second * 3


getPhaseTime: Phase -> Time.Time
getPhaseTime phase = 
        case phase of
            Working -> workTime
            SmallBreak -> smallBreakTime


getNextPhase: Phase -> Phase
getNextPhase phase = 
        if  | phase == Working -> SmallBreak
            | phase == SmallBreak -> Working



-- UPDATES

type Action = NoOp
             |UpdateTime
             |Start
             |Continue Phase
             |Stop

-- Actions outgoing from the component, similiar to requests for actions
type OutgoingAction  = RequestStart
                      |RequestContinue Phase
                      |RequestStop

update : (Time.Time, Action) -> Model -> Model
update (now, action) model = 
    case action of
        NoOp -> model

        -- Update timeLeft based on current time
        UpdateTime -> 

            if  | (model.started) && (not model.paused) && (model.timeEnding > now)  ->
                    { model |
                        timeLeft <- (model.timeEnding - now)
                    }


                -- switch phases
                | (model.started) && (not model.paused) && (model.timeEnding <= now) ->
                    let
                        pomodoroLog = {
                            timeStarted = model.timeStarted,
                            timeFinished = model.timeEnding,
                            finished = True
                        }
                        nextPhase = getNextPhase model.phase
                    in
                        { model |
                            paused <- True,
                            phase <- nextPhase,
                            timeLeft <- (getPhaseTime nextPhase),

                            log <- PomodoroLog.update ( (PomodoroLog.AppendPomodoroLog) pomodoroLog ) model.log
                        }

                | otherwise -> model
                    

        Start -> 
                { model |
                    paused <- False,
                    started <- True,
                    timeLeft <- workTime,
                    timeStarted <- now,
                    timeEnding <- (now + workTime)
                }

        Continue phase ->
                let
                    phaseTime = getPhaseTime phase
                in
                    { model | 
                        paused <- False,
                        timeLeft <- phaseTime,
                        timeStarted <- now,
                        timeEnding <- (now + phaseTime)
                    }
            

        Stop -> 
            let
                pomodoroLog = {
                    timeStarted = model.timeStarted,
                    timeFinished = now,
                    finished = False
                }
            in
                { model | 
                    paused <- False,
                    started <- False,
                    timeLeft <- workTime,
                    timeEnding <- 0,

                    log <- PomodoroLog.update ( (PomodoroLog.AppendPomodoroLog) pomodoroLog ) model.log
                }






-- VIEW


timeLeftView : Signal.Address OutgoingAction -> Model -> Html.Html
timeLeftView address model = 
    div [ 
        class "pomodoro__timeLeft" 
    ][
        h2 [] [
            text <| TimeUtils.formatTimeString <| model.timeLeft
        ]
    ]


controlsView : Signal.Address OutgoingAction -> Model -> Html.Html
controlsView address model = 
    div [ class "pomodoro__controls"] [
        if not model.started then
            button [    
                onClick address RequestStart, 
                class "pomodoro__controls__start"
            ][
                text "Start"
            ]
        else div [] []
        ,

        if model.paused then
            button [ 
                onClick address (RequestContinue model.phase),
                class "pomodoro__controls__resume"
            ][
                case model.phase of
                    Working -> text "Start working"
                    SmallBreak -> text "Start break"
            ]
        else div [] []
        ,

        if (model.started && (not model.paused) )  then
            button [ 
                onClick address RequestStop,
                class "faded pomodoro__controls__stop"
            ][
                text "Stop"
            ]
        else div [] []
    ]


view : Signal.Address OutgoingAction -> Model -> Html.Html 
view address model = 
    div [] [
        div [ class "pomodoro" ] [
            timeLeftView address model,
            controlsView address model
        ]
    ]


