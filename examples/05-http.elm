import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , error : String
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" ""
  , getRandomGif topic
  )



-- UPDATE


type Msg
  = MorePlease
  | Topic (String)
  | NewGif (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)

    Topic newTopic ->
      ({model | topic = newTopic}, Cmd.none)

    NewGif (Ok newUrl) ->
      ({model | gifUrl = newUrl, error = ""}, Cmd.none)

    NewGif (Err error) ->
      ({model | gifUrl = "", error = toString error}, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , div [] [ label [] [text "Enter a topic"]
      , input [placeholder model.topic, onInput Topic] []
      ]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    , div [] [text model.error]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
