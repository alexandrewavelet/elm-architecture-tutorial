import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String.Extra exposing (unquote)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { input : String
  , taskList : List Task
  }

type alias Task = { task : String, complete : Bool }

type Visibility = All | Active | Completed

model : Model
model =
  Model "" []

-- UPDATE


type Msg
  = InputTask String
  | Add


update : Msg -> Model -> Model
update msg {input, taskList} =
  case msg of
    InputTask newInput ->
      Model (unquote <| toString newInput) taskList

    Add ->
      Model "" ((Task input False) :: taskList)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Task list"]
    , input [type_ "text", placeholder "task", onInput InputTask, value model.input] []
    , button [onClick Add] [text "Add task"]
    , ul [] (List.map displayTask model.taskList)
    ]

displayTask : Task -> Html Msg
displayTask task =
  let
    color =
      if task.complete then
        "green"
      else
        "red"
  in
  li [style [("color", color)]] [text task.task]

