import Html exposing (..)
import Html.Attributes exposing (src, alt)
import Html.Events exposing (..)
import Random



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFace1 : Int
  , dieFace2 : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace1 Int
  | NewFace2 Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace1 (Random.int 1 6))

    NewFace1 newFace ->
      ({model | dieFace1 = newFace}, Random.generate NewFace2 (Random.int 1 6))

    NewFace2 newFace ->
      ({model | dieFace2 = newFace}, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [] [ dieFaceImage model.dieFace1 ]
    , div [] [ dieFaceImage model.dieFace2 ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

dieFaceImage : number -> Html Msg
dieFaceImage dieFace =
  let
    (fileName) =
      case dieFace of
        1 ->
          "1.png"
        2 ->
          "2.png"
        3 ->
          "3.png"
        4 ->
          "4.png"
        5 ->
          "5.png"
        6 ->
          "6.png"
        _ ->
          Debug.crash "This shouldn't happen"
  in
    img [ src ("images/" ++ fileName), alt (toString dieFace ++ " face")] []
