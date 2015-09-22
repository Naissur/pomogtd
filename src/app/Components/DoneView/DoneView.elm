module Components.DoneView.DoneView where

import Html exposing (Html, span, button, textarea, p, a, h2, h3, form, input, div, text)
import Html.Attributes exposing (attribute, value, placeholder, type', src, class, classList)
import Html.Events exposing (onClick, onSubmit)
import Date exposing (Date, toTime, fromTime)

import Components.Pomodoro.PomodoroLog as PomodoroLog
import Components.GTD.GTD as GTD

import Common.TimeUtils exposing (extractDatesUniqueToMonthDay,  haveSameDayAndMonth, getMonthDayString)

import Debug exposing (log)



type OutgoingAction = NoOpOutgoingAction
                     |GTDOutgoingAction GTD.OutgoingAction

    

                                                                             {- pomodoros count -}
doneTasksDateBoxView : Signal.Address OutgoingAction -> Date -> List GTD.Task -> Int -> Html.Html
doneTasksDateBoxView address date tasks pomodorosFinished =
        div [ class "doneTasksDateBox" ][
            
            h2 [ class "doneTasksDateBox__title" ][
                let
                    dateString = (\(m, d) -> m ++ " " ++ d ) << getMonthDayString <| date 
                in
                    span[ class "doneTasksDateBox__title__date" ][
                        text dateString
                    ],

                let
                    pomodorosFinishedString = "("++ toString pomodorosFinished ++ " pomodoros finished)"
                in
                    span[ class "doneTasksDateBox__title__pomodorosFinished" ][
                        text pomodorosFinishedString
                    ]
            ],

            div [ class "doneTasksDateBox__tasks" ]
                (tasks 
                    |> List.sortBy ( .timeDone )
                    |> List.reverse
                    |> List.map (GTD.taskView (Signal.forwardTo address GTDOutgoingAction) )
                )
        ]



doneTasksView : Signal.Address OutgoingAction -> PomodoroLog.Log -> GTD.Model -> Html.Html
doneTasksView address pomodoroLog gtdModel = 
    div [ class "doneView__doneTasksView" ][

        let 
            doneTasks = gtdModel.tasks
                     |> List.filter ( .done >> ( (==) True) )

            doneTasksCount = List.length doneTasks

            gtdTasksGroupedByDates = GTD.extractTasksGroupedByMonthDay doneTasks


            pomodoros = pomodoroLog.pomodoros
            pomodoroLogsGroupedByDates = PomodoroLog.extractLogStampsGroupedByMonthDay pomodoros
        in
            if (doneTasksCount == 0) then
                p [ class "doneView__doneTasksView__noTasks" ][
                    text "Nothing done yet!"
                ] 
            else
                div [class "doneView__doneTasksView__tasks"]
                    (gtdTasksGroupedByDates 
                        |> List.map (\(date, tasks) -> (
                            let
                                finishedPomodorosCount = PomodoroLog.getPomodorosNearDate pomodoros date
                                                      |> List.filter( ((==) True) << .finished)
                                                      |> List.length
                            in

                                doneTasksDateBoxView address date tasks finishedPomodorosCount 
                        ) ) )
    ]

doneView : Signal.Address OutgoingAction -> PomodoroLog.Log -> GTD.Model -> Html.Html
doneView address pomodoroLog gtdModel = 
    div [ class "doneView" ][
            div [ class "gtd" ][
                doneTasksView address pomodoroLog gtdModel
            ]
    ]

