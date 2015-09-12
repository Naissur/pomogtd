import Html exposing (Html, a, img, p, h3, div, button, text)
import Html.Attributes exposing (href, src, class, classList)
import Html.Events exposing (onClick)
import Signal exposing (..)
import Time exposing (fps)
import Maybe exposing (andThen)

import Common.EventUtils exposing (onClickWithPreventDefault)

import Components.Pomodoro.Pomodoro as Pomodoro
import Components.GTD.GTD as GTD


-- MODEL


type Screen =  Todo
              |Done

type alias PomoGTDModel = {
    currentScreen : Screen,
    pomodoroModel : Pomodoro.Model,
    gtdModel : GTD.Model
}

type alias SerealizedPomoGTDModel = {
    currentScreen : String,
    pomodoroModel : Pomodoro.SerealizedModel,
    gtdModel : GTD.SerealizedModel
}


serealizeScreen : Screen -> String
serealizeScreen screen = 
    case screen of
        Todo -> "Todo"
        Done -> "Done"

deSerealizeScreen : String -> Maybe Screen
deSerealizeScreen screen = 
    case screen of
        "Todo" -> Just Todo
        "Done" -> Just Done
        otherwise -> Nothing

serealizeModel : PomoGTDModel -> SerealizedPomoGTDModel
serealizeModel model = 
        {
            currentScreen = serealizeScreen model.currentScreen,
            pomodoroModel = Pomodoro.serealizeModel model.pomodoroModel,
            gtdModel = GTD.serealizeModel model.gtdModel
        }

deSerealizeModel : SerealizedPomoGTDModel -> (Maybe PomoGTDModel)
deSerealizeModel model = 
                let
                    screen = deSerealizeScreen model.currentScreen
                    pModel = (Pomodoro.deSerealizeModel model.pomodoroModel)
                    gModel = (GTD.deSerealizeModel model.gtdModel)
                in
                    case (screen, pModel, gModel) of
                        (Just s, Just p, Just g) ->

                            Just {
                                currentScreen = s,
                                pomodoroModel = p,
                                gtdModel = g
                            }
                        otherwise -> Nothing


emptyModel : PomoGTDModel
emptyModel = {
        currentScreen = Todo,
        pomodoroModel = Pomodoro.initialModel,
        gtdModel = GTD.initialModel
    }


-- UPDATES

type Action = NoOp
             |ChangeScreen Screen
             |PomodoroAction Pomodoro.Action
             |GTDAction GTD.Action

type IncomingAction = NoOpIncoming
                     |UpdatePomodoroTime
                     |RequestChangeScreen Screen
                     |PomodoroIncomingAction Pomodoro.OutgoingAction
                     |GTDIncomingAction GTD.OutgoingAction

update: (Time.Time, IncomingAction) -> PomoGTDModel -> PomoGTDModel 
update (now, action) model = 
    case action of
        NoOpIncoming -> model

        UpdatePomodoroTime -> 
                    { model |
                        pomodoroModel <- (Pomodoro.update (now, Pomodoro.UpdateTime ) model.pomodoroModel)
                    }

        RequestChangeScreen screen ->
                    { model | currentScreen <- screen }

        (GTDIncomingAction (GTD.RequestMoveTaskToTop id) ) ->              
                    { model | gtdModel <- (GTD.update (GTD.MoveTaskToTop id )  model.gtdModel) }

        (GTDIncomingAction (GTD.RequestMarkTaskAsDone id) ) ->             
                    { model | gtdModel <- (GTD.update (GTD.MarkTaskAsDone id now) model.gtdModel) }

        (GTDIncomingAction (GTD.RequestAppendTask desc) ) ->               
                    { model | gtdModel <- (GTD.update (GTD.AppendTask desc )   model.gtdModel) }

        (GTDIncomingAction (GTD.RequestRemoveTask id) ) ->                 
                    { model | gtdModel <- (GTD.update (GTD.RemoveTask id)      model.gtdModel) }

        (GTDIncomingAction (GTD.RequestUpdateNewTaskDescription desc) ) -> 
                    { model | gtdModel <- (GTD.update (GTD.UpdateNewTaskDescription desc )  model.gtdModel) }


        (PomodoroIncomingAction (Pomodoro.RequestStart)) ->
                    { model |
                        pomodoroModel <- (Pomodoro.update (now, Pomodoro.Start ) model.pomodoroModel),
                        gtdModel <- ( (GTD.update GTD.EnableFirstHighlighting) << 
                                      (GTD.update GTD.DisableNewTaskAppending) <| model.gtdModel)
                    }

        (PomodoroIncomingAction (Pomodoro.RequestContinue phase)) ->   
                    case phase of
                        Pomodoro.Working ->
                            { model |
                                pomodoroModel <- (Pomodoro.update (now, (Pomodoro.Continue phase) ) model.pomodoroModel),
                                gtdModel <- ( (GTD.update GTD.EnableFirstHighlighting) << 
                                              (GTD.update GTD.DisableNewTaskAppending) <| model.gtdModel)
                            }

                        Pomodoro.SmallBreak ->
                            { model |
                                pomodoroModel <- (Pomodoro.update (now, (Pomodoro.Continue phase) ) model.pomodoroModel),
                                gtdModel <- ( (GTD.update GTD.DisableFirstHighlighting) << 
                                              (GTD.update GTD.EnableNewTaskAppending) <| model.gtdModel)
                            }

        (PomodoroIncomingAction (Pomodoro.RequestStop)) ->      
                    { model |
                        pomodoroModel <- (Pomodoro.update (now,  (Pomodoro.Stop) ) model.pomodoroModel),
                        gtdModel <- ( (GTD.update GTD.DisableFirstHighlighting) << 
                                      (GTD.update GTD.EnableNewTaskAppending) <| model.gtdModel)
                    }



-- VIEW


todoView : Signal.Address IncomingAction -> PomoGTDModel -> Html.Html
todoView address model = 
    div [ class "main__todoView" ][
        Pomodoro.view (Signal.forwardTo address PomodoroIncomingAction ) model.pomodoroModel,
        div [ class "gtd" ][
            GTD.newTaskView (Signal.forwardTo address GTDIncomingAction ) model.gtdModel,
            GTD.tasksPoolView (Signal.forwardTo address GTDIncomingAction ) model.gtdModel
        ]
    ]

doneView : Signal.Address IncomingAction -> PomoGTDModel -> Html.Html
doneView address model = 
    div [ class "main__doneView" ][
        div [ class "gtd" ][
            GTD.doneTasksView (Signal.forwardTo address GTDIncomingAction ) model.gtdModel
        ]
    ]

view : Signal.Address IncomingAction -> PomoGTDModel -> Html.Html
view address model = 
    div [ class "wrapper" ] [
        div [ class "header" ] [
            img [ 
                src "images/logo.svg", 
                class "header__logo" 
            ][],

            a [ 
                href "", 
                classList [
                   ( "header__screenLink", True),
                   ( "active", model.currentScreen == Todo )
                ],
                onClickWithPreventDefault address (RequestChangeScreen Todo)
            ][
                text "To-do"
            ],

            a [ 
                href "", 
                classList [
                   ( "header__screenLink", True),
                   ( "active", model.currentScreen == Done )
                ],
                onClickWithPreventDefault address (RequestChangeScreen Done)
            ][
                text "Done"
            ]
        ],

        div [ 
            classList[
                ("main", True),
                ("showTodo", model.currentScreen == Todo),
                ("showDone", model.currentScreen == Done)
            ]
        ] [
            div [ 
                class "main__container"
            ][
                div [ 
                    class "main__todo"
                ][
                    todoView address model
                ],
                div [ 
                    class "main__done"
                ][
                    doneView address model
                ]
            ]
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

--handleIncomingAction : IncomingAction -> Action
--handleIncomingAction action = 
--        case action of
--            NoOpIncoming -> NoOp
--
--            RequestChangeScreen screen ->   (ChangeScreen screen)
--
--            (PomodoroIncomingAction (Pomodoro.RequestStart)) ->                (PomodoroAction (Pomodoro.Start))
--            (PomodoroIncomingAction (Pomodoro.RequestContinue phase)) ->       (PomodoroAction (Pomodoro.Continue phase))
--            (PomodoroIncomingAction (Pomodoro.RequestStop)) ->                 (PomodoroAction (Pomodoro.Stop))
--
--            (GTDIncomingAction (GTD.RequestMoveTaskToTop id) ) ->              (GTDAction (GTD.MoveTaskToTop id) )
--            (GTDIncomingAction (GTD.RequestMarkTaskAsDone id) ) ->             (GTDAction (GTD.MarkTaskAsDone id) )
--            (GTDIncomingAction (GTD.RequestAppendTask desc) ) ->               (GTDAction (GTD.AppendTask desc) )
--            (GTDIncomingAction (GTD.RequestRemoveTask id) ) ->                 (GTDAction (GTD.RemoveTask id) )
--            (GTDIncomingAction (GTD.RequestUpdateNewTaskDescription desc) ) -> (GTDAction (GTD.UpdateNewTaskDescription desc) )


updatePomodoroTimeSignal : Signal (Time.Time, IncomingAction)
updatePomodoroTimeSignal = 
              (\now -> (now, UpdatePomodoroTime) ) <~ (Time.every (Time.millisecond*500) )


modelUpdatesSignal : Signal (Time.Time, IncomingAction)
modelUpdatesSignal = Signal.merge (Time.timestamp <| incomingActions.signal ) updatePomodoroTimeSignal 



initialModel : PomoGTDModel
initialModel = Maybe.withDefault emptyModel (storedModel `andThen` deSerealizeModel )

model : Signal PomoGTDModel
model = Signal.foldp update initialModel modelUpdatesSignal


