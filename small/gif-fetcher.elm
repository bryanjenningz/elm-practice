import Html exposing (div, h2, button, text, img, br, input)
import Html.Attributes exposing (src, placeholder, disabled)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Task
import Http
import Json.Decode as Json


main =
  program
    { init = init "cats"
    , subscriptions = subscriptions
    , view = view
    , update = update
    }


type alias Model =
  { topic : String
  , gifUrl : String
  , errorMessage : String
  , isFetching : Bool
  }


type Msg
  = FetchGif
  | FetchSuccess String
  | FetchFail Http.Error
  | ChangeTopic String


init topic =
  (Model topic "waiting.gif" "" True, getRandomGif topic)


subscriptions model =
  Sub.none


view model =
  div []
    [ h2 [] [ text <| "Topic: " ++ model.topic ]
    , input [ onInput ChangeTopic, placeholder "Change topic" ] []
    , button [ onClick FetchGif, disabled model.isFetching ] [ text "Get GIF!" ]
    , br [] []
    , img [ src model.gifUrl ] []
    , text model.errorMessage
    ]


update msg model =
  case msg of
    FetchGif ->
      ({ model | isFetching = True }, getRandomGif model.topic)
    
    FetchSuccess newUrl ->
      (Model model.topic newUrl "" False, Cmd.none)
    
    FetchFail error ->
      ({ model | errorMessage = (toString error), isFetching = False }, Cmd.none)
     
    ChangeTopic topic ->
      ({ model | topic = topic }, Cmd.none)


getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSuccess (Http.get decodeGifUrl url)


decodeGifUrl =
  Json.at ["data", "image_url"] Json.string
