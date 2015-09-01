module Components.GTD.GTD where

import Html exposing (Html, a, form, input, div, text)
import Html.Attributes exposing (value, placeholder, type', src, class, classList)
import Html.Events exposing (onClick, onSubmit)
import Signal exposing (..)
import Time exposing (fps)

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
    enableNewTaskAdding : Bool
}

initialModel : Model
initialModel =  {
                    tasks = [],
                    newTaskDescription = "",
                    enableNewTaskAdding = True
                }

-- UPDATES

type Action = NoOp
             |RemoveTask TaskId
             |AppendTask String
             |EnableNewTaskAppenging
             |DisableNewTaskAppending
             |UpdateNewTaskDescription String

type OutgoingAction = NoOpOutgoingAction
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

            EnableNewTaskAppenging -> 
                { model |
                    enableNewTaskAdding <- True
                }

            UpdateNewTaskDescription desc ->
                {model |
                    newTaskDescription <- desc
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
    div [ class "gtd__newTask" ] [
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
        div [ class "gtd__task__description" ][
            text task.description
        ],

        div [ 
            class "gtd__task__remove",
            onClick address (RequestRemoveTask task.id)
        ][
            text "x"
        ]
    ]

tasksPoolView : Signal.Address OutgoingAction -> Model -> Html.Html
tasksPoolView address model = 
    div [ class "gtd__tasksPoolView" ][

        div [ class "gtd__tasksPoolView__tasks" ]
            (List.map (taskView address) model.tasks)
    ]

view : Signal.Address OutgoingAction -> Model -> Html.Html
view address model = 
    div [class "gtd" ] [
        newTaskView address model,
        tasksPoolView address model
    ]



