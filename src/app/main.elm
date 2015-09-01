import Html exposing (Html, img, p, h3, div, button, text)
import Html.Attributes exposing (src, class, classList)
import Signal exposing (..)
import Time exposing (fps)


import Components.Pomodoro.Pomodoro as Pomodoro
import Components.GTD.GTD as GTD


-- MODEL


type alias PomoGTDModel = {
    pomodoroModel : Pomodoro.Model
}

initialModel : PomoGTDModel
initialModel = {
        pomodoroModel = Pomodoro.initialModel
    }



-- UPDATES
type Action = NoOp
             |PomodoroAction Pomodoro.Action

type IncomingAction = NoOpIncoming
                     |PomodoroIncomingAction Pomodoro.OutgoingAction

update: Action -> PomoGTDModel -> PomoGTDModel 
update action model = 
    case action of
        NoOp -> model
        (PomodoroAction pomoAction) -> 
                    { model |
                        pomodoroModel <- (Pomodoro.update pomoAction model.pomodoroModel)
                    }



-- VIEW

view : Signal.Address IncomingAction -> PomoGTDModel -> Html.Html
view address model = 
    div [ class "wrapper" ] [
        div [ class "header" ] [
            img [ src "/images/logo.svg", class "header__logo" ] [ ]
        ],

        div [ class "main" ] [
            Pomodoro.view (Signal.forwardTo address PomodoroIncomingAction ) model.pomodoroModel
        ]
    ]





-- WIRING UP

main : Signal Html
main = view incomingActions.address <~ model


pausedPort: Signal Bool
pausedPort = Signal.dropRepeats <| (.paused << .pomodoroModel) <~ model

port soundPort : Signal String
port soundPort = (\paused -> if paused then "ring" else "") <~ pausedPort





incomingActions : Signal.Mailbox IncomingAction
incomingActions = Signal.mailbox NoOpIncoming

handleIncomingAction : (Time.Time, IncomingAction) -> Action
handleIncomingAction (now, action) = 
        case action of
            NoOpIncoming -> NoOp
            (PomodoroIncomingAction (Pomodoro.RequestStart)) ->      (PomodoroAction ((Pomodoro.Start)    now))
            (PomodoroIncomingAction (Pomodoro.RequestContinue)) ->   (PomodoroAction ((Pomodoro.Continue) now))
            (PomodoroIncomingAction (Pomodoro.RequestStop)) ->       (PomodoroAction ((Pomodoro.Stop) now))


updatePomodoroTimeSignal : Signal Action
updatePomodoroTimeSignal = 
              (\now -> (PomodoroAction (Pomodoro.UpdateTime now)) ) <~ (Time.every Time.second)


modelUpdatesSignal : Signal Action
modelUpdatesSignal = Signal.merge (handleIncomingAction <~ (Time.timestamp incomingActions.signal) ) updatePomodoroTimeSignal 

model : Signal PomoGTDModel
model = Signal.foldp update initialModel modelUpdatesSignal




