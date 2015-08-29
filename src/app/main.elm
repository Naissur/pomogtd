import Html exposing (Html, p, h3, div, button, text)
import Html.Attributes exposing (class, classList)
import Signal exposing (..)
import Time exposing (fps)


import Components.Pomodoro.Pomodoro as Pomodoro




-- MODEL


type alias PomoGTDModel = {
    pomodoroModel : Pomodoro.Model
}


-- UPDATES
type Action = NoOp
             |PomodoroAction Pomodoro.Action

update: Action -> PomoGTDModel -> PomoGTDModel 
update action model = 
    case action of
        NoOp -> model
        (PomodoroAction pomoAction) -> { model |
                                            pomodoroModel <- (Pomodoro.update pomoAction model.pomodoroModel)
                                       }



-- VIEW

view : Signal.Address Action -> PomoGTDModel -> Html.Html
view address model = 
    div [] [
        div [ class "header" ] [
            text "HEADER"
        ],

        div [ class "main" ] [
            Pomodoro.view (Signal.forwardTo address PomodoroAction ) model.pomodoroModel
        ]
    ]



-- WIRING UP

main : Signal Html
main = view actions.address <~ model

updatePomodoroTimeSignal : Signal Action
updatePomodoroTimeSignal = 
              (\dt -> PomodoroAction (Pomodoro.UpdateTime dt) ) <~ (fps 6)

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


model : Signal PomoGTDModel
model =
    Signal.foldp update initialModel (Signal.merge actions.signal updatePomodoroTimeSignal )


initialModel : PomoGTDModel
initialModel = {
        pomodoroModel = Pomodoro.initialModel
    }





