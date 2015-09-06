module Components.GTD.GTD where

import Html exposing (Html, p, a, h2, h3, form, input, div, text)
import Html.Attributes exposing (attribute, value, placeholder, type', src, class, classList)
import Html.Events exposing (onClick, onSubmit)
import Signal exposing (..)
import Time exposing (fps)
import Date exposing (..)
import List exposing(length, append)

import Common.EventUtils exposing (onInput, onSubmitPreventDefault)
import Common.TimeUtils exposing (getMonthString)


-- MODEL

type alias TaskId = Int

type alias Task = {
    id : TaskId,
    done : Bool,
    timeDone : Time.Time,
    description : String
}


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
             |RemoveTask TaskId
             |MarkTaskAsDone TaskId Time.Time
             |AppendTask String
             |MoveTaskToTop TaskId 
             |EnableNewTaskAppending
             |DisableNewTaskAppending
             |EnableFirstHighlighting
             |DisableFirstHighlighting
             |UpdateNewTaskDescription String

type OutgoingAction = NoOpOutgoingAction
                     |RequestMoveTaskToTop TaskId
                     |RequestMarkTaskAsDone TaskId
                     |RequestRemoveTask TaskId
                     |RequestAppendTask String
                     |RequestUpdateNewTaskDescription String


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
                                                        } else 
                                                        task
                                        ) model.tasks
                in
                    { model | tasks <- newTasks }
                
            AppendTask taskDescription ->
                let
                    newId = model.lastId + 1
                    newTask = {
                        id = newId,
                        done = False,
                        timeDone = 0,
                        description = taskDescription
                    }
                in
                    { model |
                        lastId <- newId,
                        tasks <-  model.tasks ++ [newTask],
                        newTaskDescription <- ""
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
    div [ class "gtd__task" ][
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
    div [ class "gtd__task" ][
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

        div [ class "gtd__task__description" ][
            text task.description
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

    

doneTasksView : Signal.Address OutgoingAction -> Model -> Html.Html
doneTasksView address model = 
    div [ class "gtd__doneTasksView" ][

        h2 [ class "gtd__doneTasksView__title" ][
            text "Done"
        ],

        let 
            doneTasksCount =   model.tasks
                            |> List.filter ( .done >> ( (==) True) )
                            |> length
        in
            if (doneTasksCount == 0) then
                p [ class "gtd__doneTasksView__noTasks" ][
                    text "Nothing done yet!"
                ] 
            else
                div [ class "gtd__doneTasksView__tasks" ]
                    (model.tasks 
                        |> List.filter ( .done >> ( (==) True) )
                        |> List.sortBy ( .timeDone )
                        |> List.map (taskView address)
                    )
    ]
