import Html exposing (Html, img, p, h3, div, button, text)
import Html.Attributes exposing (src, class, classList)
import Signal exposing (..)
import Time exposing (fps)
import Maybe exposing (andThen)


import Components.Pomodoro.Pomodoro as Pomodoro
import Components.GTD.GTD as GTD


-- MODEL


type alias PomoGTDModel = {
    pomodoroModel : Pomodoro.Model,
    gtdModel : GTD.Model
}

type alias SerealizedPomoGTDModel = {
    pomodoroModel : Pomodoro.SerealizedModel,
    gtdModel : GTD.SerealizedModel
}


serealizeModel : PomoGTDModel -> SerealizedPomoGTDModel
serealizeModel model = 
        {
            pomodoroModel = Pomodoro.serealizeModel model.pomodoroModel,
            gtdModel = GTD.serealizeModel model.gtdModel
        }

deSerealizeModel : SerealizedPomoGTDModel -> (Maybe PomoGTDModel)
deSerealizeModel model = 
                let
                    pModel = (Pomodoro.deSerealizeModel model.pomodoroModel)
                    gModel = (GTD.deSerealizeModel model.gtdModel)
                in
                    case (pModel, gModel) of
                        (Just p, Just g) ->

                            Just {
                                pomodoroModel = p,
                                gtdModel = g
                            }
                        otherwise -> Nothing


emptyModel : PomoGTDModel
emptyModel = {
        pomodoroModel = Pomodoro.initialModel,
        gtdModel = GTD.initialModel
    }


-- UPDATES
type Action = NoOp
             |PomodoroAction Pomodoro.Action
             |GTDAction GTD.Action

type IncomingAction = NoOpIncoming
                     |PomodoroIncomingAction Pomodoro.OutgoingAction
                     |GTDIncomingAction GTD.OutgoingAction

update: (Time.Time, Action) -> PomoGTDModel -> PomoGTDModel 
update (now, action) model = 
    case action of
        NoOp -> model
        (GTDAction gtdAction) -> 
                    { model |
                        gtdModel <- (GTD.update gtdAction model.gtdModel)
                    }

        (PomodoroAction pomoAction) -> 
            case pomoAction of
                Pomodoro.Start -> 
                    { model |
                        pomodoroModel <- (Pomodoro.update (now, pomoAction) model.pomodoroModel),
                        gtdModel <- ( (GTD.update GTD.EnableFirstHighlighting) << (GTD.update GTD.DisableNewTaskAppending) <| model.gtdModel)
                    }

                Pomodoro.Continue phase -> 
                    case phase of
                        Pomodoro.Working ->
                            { model |
                                pomodoroModel <- (Pomodoro.update (now, pomoAction) model.pomodoroModel),
                                gtdModel <- ( (GTD.update GTD.EnableFirstHighlighting) << (GTD.update GTD.DisableNewTaskAppending) <| model.gtdModel)
                            }

                        Pomodoro.SmallBreak ->
                            { model |
                                pomodoroModel <- (Pomodoro.update (now, pomoAction) model.pomodoroModel),
                                gtdModel <- ( (GTD.update GTD.DisableFirstHighlighting) << (GTD.update GTD.EnableNewTaskAppending) <| model.gtdModel)
                            }

                Pomodoro.Stop -> 
                    { model |
                        pomodoroModel <- (Pomodoro.update (now, pomoAction) model.pomodoroModel),
                        gtdModel <- ( (GTD.update GTD.DisableFirstHighlighting) << (GTD.update GTD.EnableNewTaskAppending) <| model.gtdModel)
                    }

                otherwise ->
                    { model |
                        pomodoroModel <- (Pomodoro.update (now, pomoAction) model.pomodoroModel)
                    }



-- VIEW

view : Signal.Address IncomingAction -> PomoGTDModel -> Html.Html
view address model = 
    div [ class "wrapper" ] [
        div [ class "header" ] [
            img [ src "/images/logo.svg", class "header__logo" ] [ ]
        ],

        div [ class "main" ] [
            Pomodoro.view (Signal.forwardTo address PomodoroIncomingAction ) model.pomodoroModel,
            GTD.view (Signal.forwardTo address GTDIncomingAction ) model.gtdModel
        ]
    ]





-- WIRING UP

main : Signal Html
main = view incomingActions.address <~ model


pausedPort: Signal Bool
pausedPort = Signal.dropRepeats <| (.paused << .pomodoroModel) <~ model

port soundPort : Signal String
port soundPort = (\paused -> if paused then "ring" else "") <~ pausedPort

port appModelPort : Signal SerealizedPomoGTDModel
port appModelPort = Signal.dropRepeats <| serealizeModel <~ model

port storedModel : Maybe SerealizedPomoGTDModel






incomingActions : Signal.Mailbox IncomingAction
incomingActions = Signal.mailbox NoOpIncoming

handleIncomingAction : IncomingAction -> Action
handleIncomingAction action = 
        case action of
            NoOpIncoming -> NoOp

            (PomodoroIncomingAction (Pomodoro.RequestStart)) ->      (PomodoroAction (Pomodoro.Start))
            (PomodoroIncomingAction (Pomodoro.RequestContinue phase)) ->   (PomodoroAction (Pomodoro.Continue phase))
            (PomodoroIncomingAction (Pomodoro.RequestStop)) ->       (PomodoroAction (Pomodoro.Stop))

            (GTDIncomingAction (GTD.RequestMoveTaskToTop id) ) ->              (GTDAction (GTD.MoveTaskToTop id) )
            (GTDIncomingAction (GTD.RequestAppendTask desc) ) ->               (GTDAction (GTD.AppendTask desc) )
            (GTDIncomingAction (GTD.RequestRemoveTask id) ) ->                 (GTDAction (GTD.RemoveTask id) )
            (GTDIncomingAction (GTD.RequestUpdateNewTaskDescription desc) ) -> (GTDAction (GTD.UpdateNewTaskDescription desc) )


updatePomodoroTimeSignal : Signal (Time.Time, Action)
updatePomodoroTimeSignal = 
              (\now -> (now, PomodoroAction (Pomodoro.UpdateTime)) ) <~ (Time.every (Time.millisecond*500) )


modelUpdatesSignal : Signal (Time.Time, Action)
modelUpdatesSignal = Signal.merge (Time.timestamp <| handleIncomingAction <~ incomingActions.signal ) updatePomodoroTimeSignal 



initialModel : PomoGTDModel
initialModel = Maybe.withDefault emptyModel (storedModel `andThen` deSerealizeModel )

model : Signal PomoGTDModel
model = Signal.foldp update initialModel modelUpdatesSignal


