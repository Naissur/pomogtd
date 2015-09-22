module Components.DoneView.DoneView where

import Html exposing (Html, button, textarea, p, a, h2, h3, form, input, div, text)
import Html.Attributes exposing (attribute, value, placeholder, type', src, class, classList)
import Html.Events exposing (onClick, onSubmit)
import Date exposing (Date, toTime, fromTime)

import Components.Pomodoro.Pomodoro as Pomodoro
import Components.GTD.GTD as GTD

import Common.TimeUtils exposing (extractDatesUniqueToMonthDay,  haveSameDayAndMonth, getMonthDayString)



type OutgoingAction = NoOpOutgoingAction
                     |GTDOutgoingAction GTD.OutgoingAction

    

doneTasksDateBoxView : Signal.Address OutgoingAction -> Date -> List GTD.Task -> Html.Html
doneTasksDateBoxView address date tasks =
        div [ class "gtd__doneTasksDateBox" ][
            
            h2 [ class "gtd__doneTasksDateBox__date" ][
                text <| (\(m, d) -> m ++ " " ++ d ) <| getMonthDayString <| date 
            ],

            div [ class "gtd__doneTasksDateBox__tasks" ]
                (tasks 
                    |> List.sortBy ( .timeDone )
                    |> List.reverse
                    |> List.map (GTD.taskView (Signal.forwardTo address GTDOutgoingAction) )
                )
        ]



doneTasksView : Signal.Address OutgoingAction -> GTD.Model -> Html.Html
doneTasksView address model = 
    div [ class "doneView__doneTasksView" ][

        let 
            doneTasks = model.tasks
                     |> List.filter ( .done >> ( (==) True) )

            doneTasksCount = List.length doneTasks

            groupedByDates = GTD.extractTasksGroupedByMonthDay doneTasks
        in
            if (doneTasksCount == 0) then
                p [ class "doneView__doneTasksView__noTasks" ][
                    text "Nothing done yet!"
                ] 
            else
                div [class "doneView__doneTasksView__tasks"]
                    (groupedByDates 
                        |> List.map (\(date, tasks) -> (doneTasksDateBoxView address date tasks ) ) )
    ]

doneView : Signal.Address OutgoingAction -> Pomodoro.Model -> GTD.Model -> Html.Html
doneView address pomodoroModel gtdModel = 
    div [ class "doneView" ][
        div [ class "gtd" ][
            doneTasksView address gtdModel
        ]
    ]

