module Components.GTD.GTD where

import Html exposing (Html, button, textarea, p, a, h2, h3, form, input, div, text)
import Html.Attributes exposing (attribute, value, placeholder, type', src, class, classList)
import Html.Events exposing (onClick, onSubmit)
import Signal exposing (..)
import Time exposing (fps)
import Date exposing (Date, toTime, fromTime)
import List exposing(length, append)
import Maybe exposing (withDefault, map)
import Debug

import Common.EventUtils exposing (onChange, onInput, onSubmitPreventDefault)
import Common.TimeUtils exposing (extractDatesUniqueToMonthDay,  haveSameDayAndMonth, getMonthDayString)


-- MODEL

type alias TaskId = Int

type alias Task = {
    id : TaskId,
    editing: Bool,
    done : Bool,
    timeDone : Time.Time,
    description : String,
    pendingDescription : String
}


getTasksNearDate : List Task -> Date -> List Task
getTasksNearDate tasks date = 
                           tasks 
                           |> List.filter ( (haveSameDayAndMonth date) << fromTime << .timeDone ) 

extractTasksGroupedByMonthDay : List Task -> List (Date, (List Task) )
extractTasksGroupedByMonthDay tasks = 
                    let
                        dates = extractDatesUniqueToMonthDay << List.map (fromTime << .timeDone) <| tasks
                    in
                        List.map ( \date -> (date, getTasksNearDate tasks date)) dates
                        


type alias Model = {
    tasks : List Task,
    lastId : Int,
    newTaskDescription : String,
    enableNewTaskAdding : Bool,
    highlightFirst : Bool
}

type alias SerealizedModel = {
    tasks : List Task,
    lastId : Int,
    newTaskDescription : String,
    enableNewTaskAdding : Bool,
    highlightFirst : Bool
}

serealizeModel : Model -> SerealizedModel
serealizeModel model = model

deSerealizeModel : SerealizedModel -> Maybe Model
deSerealizeModel model = Just model

initialModel : Model
initialModel =  {
                    lastId = 0,
                    tasks = [],
                    newTaskDescription = "",
                    enableNewTaskAdding = True,
                    highlightFirst = False
                }




-- UPDATES

type Action = NoOp

             |StartEditingTaskDescription TaskId
             |UpdateEditingTaskDescription TaskId String
             |SaveEditedTaskDescription TaskId
             |CancelEditingTaskDescription TaskId

             |UpdateNewTaskDescription String

             |AppendTask String
             |RemoveTask TaskId
             |MarkTaskAsDone TaskId Time.Time
             |MoveTaskToTop TaskId 

             |EnableNewTaskAppending
             |DisableNewTaskAppending
             |EnableFirstHighlighting
             |DisableFirstHighlighting

type OutgoingAction = NoOpOutgoingAction
                     |RequestChangePendingDescriptionForTask TaskId String
                     |RequestStartEditingTaskDescription TaskId
                     |RequestUpdateEditingTaskDescription TaskId String
                     |RequestSaveEditedTaskDescription TaskId
                     |RequestCancelEditingTaskDescription TaskId

                     |RequestUpdateNewTaskDescription String
                     |RequestAppendTask String
                     |RequestRemoveTask TaskId

                     |RequestMoveTaskToTop TaskId
                     |RequestMarkTaskAsDone TaskId


update : Action -> Model -> Model
update action model = 
        case action of 
            NoOp -> model

            DisableNewTaskAppending -> 
                { model | enableNewTaskAdding <- False }

            EnableNewTaskAppending -> 
                { model | enableNewTaskAdding <- True }

            EnableFirstHighlighting ->
                { model | highlightFirst <- True }

            DisableFirstHighlighting ->
                { model | highlightFirst <- False }

            UpdateNewTaskDescription desc ->
                { model | newTaskDescription <- desc }

            MoveTaskToTop taskId -> 
                let
                    taskToMove = List.filter (( (==) taskId) << .id) model.tasks
                    newTasks = List.filter (( (/=) taskId) << .id) model.tasks
                in
                    {model | tasks <- append taskToMove newTasks }

            RemoveTask taskId ->
                let
                    newTasks = List.filter (( (/=) taskId) << .id) model.tasks
                in
                    { model | tasks <- newTasks }
                
            MarkTaskAsDone taskId now ->
                let
                    newTasks = List.map (
                                            \task -> if task.id == taskId then 
                                                        {task | 
                                                            done <- True,
                                                            timeDone <- now
                                                        } 
                                                     else 
                                                        task
                                        ) model.tasks
                in
                    { model | tasks <- newTasks }
                
            AppendTask taskDescription ->
                if taskDescription == "" then model else
                    let
                        newId = model.lastId + 1
                        newTask = {
                            id = newId,
                            done = False,
                            editing = False,
                            timeDone = 0,
                            description = taskDescription,
                            pendingDescription = ""
                        }
                    in
                        { model |
                            lastId <- newId,
                            tasks <-  model.tasks ++ [newTask],
                            newTaskDescription <- ""
                        }

            StartEditingTaskDescription taskId -> 
                    {model | tasks <-  model.tasks
                                    |> List.map (
                                           \task -> if (task.id == taskId) then
                                                        {task | 
                                                            editing <- True,
                                                            pendingDescription <- task.description
                                                        }
                                                    else 
                                                        task
                                       )
                                    }

            UpdateEditingTaskDescription taskId desc ->
                    {model | tasks <-  model.tasks
                                    |> List.map (
                                           \task -> if (task.id == taskId) then
                                                        {task | 
                                                            pendingDescription <- desc
                                                        }
                                                    else 
                                                        task
                                       )
                                    }

            SaveEditedTaskDescription taskId -> 
                    {model | tasks <-  model.tasks
                                    |> List.map (
                                           \task -> if (task.id == taskId) then
                                                        {task | 
                                                            editing <- False,
                                                            description <- task.pendingDescription,
                                                            pendingDescription <- ""
                                                        }
                                                    else 
                                                        task
                                       )
                                    }

            CancelEditingTaskDescription taskId -> 
                    {model | tasks <-  model.tasks
                                    |> List.map (
                                           \task -> if (task.id == taskId) then
                                                        {task | 
                                                            editing <- False,
                                                            pendingDescription <- ""
                                                        }
                                                    else 
                                                        task
                                       )
                                    }

-- VIEW


newTaskView : Signal.Address OutgoingAction -> Model -> Html.Html
newTaskView address model = 
    div [ 
        classList [
            ("gtd__newTask", True),
            ("collapsed", not model.enableNewTaskAdding),
            ("expanded", model.enableNewTaskAdding)
        ]
    ] [
        form [
            onSubmitPreventDefault address (RequestAppendTask model.newTaskDescription)
        ][
            input [ 
                class "gtd__newTask__input",
                type' "text",
                placeholder "New task...",
                value model.newTaskDescription,

                onInput address (\desc -> (RequestUpdateNewTaskDescription desc) )
            ][
            ]
        ]
    ]


doneTaskView : Signal.Address OutgoingAction -> Task -> Html.Html
doneTaskView address task = 
    div [ class "gtd__task done" ][
        div [ 
            class "gtd__task__controls"
        ][
        
            div [ class "gtd__task__controls__itemContainer" ][
                div [ 
                    class "gtd__task__controls__item gtd__task__controls__remove",
                    onClick address (RequestRemoveTask task.id)
                ][]
            ]
        ],

        div [ class "gtd__task__description" ][
            text task.description
        ]

    ]

undoneTaskView : Signal.Address OutgoingAction -> Task -> Html.Html
undoneTaskView address task = 
    div [ 
        classList [
            ("gtd__task", True),
            ("editing", task.editing)
        ]
    ][
        div [ 
            class "gtd__task__controls"
        ][
        
            div [ class "gtd__task__controls__itemContainer" ][
                div [ 
                    class "gtd__task__controls__item gtd__task__controls__moveUp",
                    onClick address (RequestMoveTaskToTop task.id)
                ][]
            ],

            div [ class "gtd__task__controls__itemContainer" ][
                div [ 
                    class "gtd__task__controls__item gtd__task__controls__markAsDone",
                    onClick address (RequestMarkTaskAsDone task.id)
                ][]
            ]
        ],

        div [ 
            class "gtd__task__description",
            onClick address (RequestStartEditingTaskDescription task.id),
            onInput address (\desc -> RequestChangePendingDescriptionForTask task.id desc)
        ][
            text task.description
        ],

        div[
            class "gtd__task__description__editing"
        ][
            textarea [ 
                class "gtd__task__description__editing__input",
                value task.pendingDescription,
                onChange address (\desc -> RequestUpdateEditingTaskDescription task.id desc)
            ][
            ],

            button [
                class "gtd__task__description__editing__submit alternative",
                onClick address (RequestSaveEditedTaskDescription task.id)
            ][
                text "OK"
            ],

            button [
                class "gtd__task__description__editing__cancel shaded",
                onClick address (RequestCancelEditingTaskDescription task.id)
            ][
                text "Cancel"
            ]
        ]

    ]

taskView : Signal.Address OutgoingAction -> Task -> Html.Html
taskView address task = 
    case task.done of
        True -> doneTaskView address task
        False -> undoneTaskView address task

tasksPoolView : Signal.Address OutgoingAction -> Model -> Html.Html
tasksPoolView address model = 
    div [ class "gtd__tasksPoolView" ][

        div [ 
            classList [
                ("gtd__tasksPoolView__tasks", True),
                ("highlightFirst", model.highlightFirst)
            ]
        ]
            (model.tasks 
                |> List.filter ( .done >> ( (==) False) ) 
                |> List.map (taskView address) 
            )
    ]

