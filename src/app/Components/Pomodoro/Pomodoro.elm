module Components.Pomodoro.Pomodoro where

import Html exposing (Html, p, h2, div, button, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Signal exposing (..)
import Time exposing (..)

import Common.TimeUtils as TimeUtils



-- MODEL

type Phase = Working
            |SmallBreak

type alias Model = {
    started: Bool,
    paused: Bool,
    phase: Phase,
    timeLeft: Time.Time
}


workTime: Time.Time
workTime = Time.minute * 25
--workTime = Time.second * 2

smallBreakTime: Time.Time
smallBreakTime  = Time.minute * 5
--smallBreakTime  = Time.second * 2

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


initialModel : Model
initialModel = {
        started = False,
        paused = False,
        phase = Working,
        timeLeft = workTime
    }


-- UPDATES

type Action = NoOp
             |UpdateTime Time.Time
             |Start
             |Continue
             |Stop

update : Action -> Model -> Model
update action model = 
    case action of
        NoOp -> model

        UpdateTime dt -> 
            if  | (model.started) && (not model.paused) && (model.timeLeft > 0)  ->
                    { model |
                        timeLeft <- (model.timeLeft - dt)
                    }


                -- switch phases
                | (model.started) && (not model.paused) && (model.timeLeft <= 0) ->
                    let
                        nextPhase = getNextPhase model.phase
                    in
                        { model |
                            paused <- True,
                            phase <- nextPhase,
                            timeLeft <- (getPhaseTime nextPhase)
                        }

                | otherwise -> model
                    

        Start -> 
                { model |
                    paused <- False,
                    started <- True,
                    timeLeft <- workTime
                }

        Continue ->
                { model | 
                    paused <- False
                }
            

        Stop -> 
            { model | 
                paused <- False,
                started <- False,
                timeLeft <- workTime
            }


-- VIEW


timeLeftView : Signal.Address Action -> Model -> Html.Html
timeLeftView address model = 
    div [ 
        class "pomodoro__timeLeft" 
    ][
        h2 [] [
            text <| TimeUtils.formatTimeString <| model.timeLeft
        ]
    ]


controlsView : Signal.Address Action -> Model -> Html.Html
controlsView address model = 
    div [ class "pomodoro__controls"] [
        if not model.started then
            button [    
                onClick address Start, 
                class "pomodoro__controls__start"
            ][
                text "Start"
            ]
        else div [] []
        ,

        if model.paused then
            button [ 
                onClick address Continue,
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
                onClick address Stop,
                class "pomodoro__controls__stop"
            ][
                text "Stop"
            ]
        else div [] []
    ]


view : Signal.Address Action -> Model -> Html.Html 
view address model = 
    div [] [
        div [ class "pomodoro" ] [
            timeLeftView address model,
            controlsView address model
        ]
    ]
