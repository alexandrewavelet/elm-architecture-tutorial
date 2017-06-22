import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex
import Char

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  , displayValidation : Html Msg
  }


model : Model
model =
  Model "" "" "" "" (div [] [])



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Validate


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Validate ->
      { model | displayValidation = viewValidation model }

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "numeric", placeholder "Age", onInput Age ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , div []
      [ button [ onClick Validate ] [ text "Validate input" ]
      ]
    , model.displayValidation
    ]

viewValidation : Model -> Html msg
viewValidation model =
  div [ style [("border", "1px solid back")]]
  [ h1 [] [text "Form validation"]
  , loginValidation model
  , ageValidation model
  ]

loginValidation : Model -> Html msg
loginValidation model =
  let
    (color, message) =
      if model.password /= model.passwordAgain then
        ("red", "Passwords do not match!")
      else if String.length model.password < 8 then
        ("red", "Password too small")
      else if not (Regex.contains (Regex.regex "[A-Z]") model.password) then
        ("red", "Password should contain at least 1 uppercase letter")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text (message) ]

ageValidation : Model -> Html msg
ageValidation model =
  let
    (color, message) =
      if String.length model.age == 0 then
        ("", "")
      else if String.all Char.isDigit model.age then
        ("green", "Age OK")
      else
        ("red", "NOOOOO")
  in
    div [ style [("color", color)] ] [ text message ]
