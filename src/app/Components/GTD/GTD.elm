module Components.GTD.GTD where

import Html exposing (Html, a, form, input, div, text)
import Html.Attributes exposing (attribute, value, placeholder, type', src, class, classList)
import Html.Events exposing (onClick, onSubmit)
import Signal exposing (..)
import Time exposing (fps)
import List exposing(append)

import Common.EventUtils exposing (onInput, onSubmitPreventDefault)


-- MODEL

type alias TaskId = Int

type alias Task = {
    id : TaskId,
    description : String
}


type alias Model = {
    tasks : List Task,
    newTaskDescription : String,
    enableNewTaskAdding : Bool,
    highlightFirst : Bool
}

type alias SerealizedModel = {
    tasks : List Task,
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
                    tasks = [],
                    newTaskDescription = "",
                    enableNewTaskAdding = True,
                    highlightFirst = False
                }

-- UPDATES

type Action = NoOp
             |RemoveTask TaskId
             |AppendTask String
             |MoveTaskToTop TaskId 
             |EnableNewTaskAppending
             |DisableNewTaskAppending
             |EnableFirstHighlighting
             |DisableFirstHighlighting
             |UpdateNewTaskDescription String

type OutgoingAction = NoOpOutgoingAction
                     |RequestMoveTaskToTop TaskId
                     |RequestRemoveTask TaskId
                     |RequestAppendTask String
                     |RequestUpdateNewTaskDescription String


update : Action -> Model -> Model
update action model = 
        case action of 
            NoOp -> model

            DisableNewTaskAppending -> 
                { model |
                    enableNewTaskAdding <- False
                }

            EnableNewTaskAppending -> 
                { model |
                    enableNewTaskAdding <- True
                }

            EnableFirstHighlighting ->
                { model |
                    highlightFirst <- True
                }

            DisableFirstHighlighting ->
                { model |
                    highlightFirst <- False
                }

            UpdateNewTaskDescription desc ->
                {model |
                    newTaskDescription <- desc
                }

            MoveTaskToTop taskId -> 
                let
                    taskToMove = List.filter (( (==) taskId) << .id) model.tasks
                    newTasks = List.filter (( (/=) taskId) << .id) model.tasks
                in
                    {model |
                        tasks <- append taskToMove newTasks
                    }

            RemoveTask taskId ->
                let
                    newTasks = List.filter (( (/=) taskId) << .id) model.tasks
                in
                    {model |
                        tasks <- newTasks
                    }
                
            AppendTask taskDescription ->
                let
                    newId = List.length model.tasks
                    newTask = {
                        id = newId,
                        description = taskDescription
                    }
                in
                    {model |
                        tasks <-  newTask :: model.tasks,
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


taskView : Signal.Address OutgoingAction -> Task -> Html.Html
taskView address task = 
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
                    onClick address (RequestRemoveTask task.id)
                ][]
            ]
            --,

            --div [ class "gtd__task__controls__itemContainer" ][
            --    div [ 
            --        class "gtd__task__controls__item gtd__task__controls__remove",
            --        onClick address (RequestRemoveTask task.id)
            --    ][ ]
            --]

        ],

        div [ class "gtd__task__description" ][
            text task.description
        ]

    ]

tasksPoolView : Signal.Address OutgoingAction -> Model -> Html.Html
tasksPoolView address model = 
    div [ class "gtd__tasksPoolView" ][

        div [ 
            classList [
                ("gtd__tasksPoolView__tasks", True),
                ("highlightFirst", model.highlightFirst)
            ]
        ]
            (List.map (taskView address) model.tasks)
    ]

view : Signal.Address OutgoingAction -> Model -> Html.Html
view address model = 
    div [class "gtd" ] [
        newTaskView address model,
        tasksPoolView address model
    ]



