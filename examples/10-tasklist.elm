import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String.Extra exposing (unquote)
import List.Extra exposing (updateIf)

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
  , currentId : Int
  }

type alias Task = { id : Int, task : String, complete : Bool }

type Visibility = All | Active | Completed

model : Model
model =
  Model "" [] 0

-- UPDATE


type Msg
  = InputTask String
  | Add
  | MarkComplete Int String


update : Msg -> Model -> Model
update msg {input, taskList, currentId} =
  case msg of
    InputTask newInput ->
      -- Unquote - BCUZ IT WORKS - PROBLEM!?
      Model (toString newInput |> unquote) taskList currentId

    Add ->
      Model
        ""
        ((newTask currentId input)
          |> addTask taskList
        )
        (increment currentId)

    MarkComplete id task->
      -- Find Task and update
      let
        updatedTask =
          Task id task True
      in
        Model input (updateIf (\task -> task.id == id) (\task -> { task | complete = True }) taskList) currentId

increment : Int -> Int
increment int =
  int + 1

newTask : Int -> String -> Task
newTask id name =
  Task id name False

addTask : List Task -> Task -> List Task
addTask list task =
  task :: list

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
  li [style [("color", color)]]
    [text ((toString task.task |> unquote) ++ " - ")
    , displayCompletionButton task
    ]

displayCompletionButton : Task -> Html Msg
displayCompletionButton task =
  if not task.complete then
    a [onClick (MarkComplete task.id task.task)] [text "mark as complete"]
  else
    text ""
