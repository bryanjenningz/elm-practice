import Html exposing (div, text, input, button)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onInput, onClick)
import Html.App exposing (program)
import WebSocket


main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


echoServer =
  "wss://echo.websocket.org"


type Msg
  = Input String
  | Send
  | NewMessage String


type alias Model =
  { input : String
  , messages : List String
  }


init =
  (Model "" [], Cmd.none)


view model =
  div []
    [ div [messageBoxStyle] (List.reverse <| List.map viewMessage model.messages)
    , input [onInput Input, value model.input] []
    , button [onClick Send] [text "Send"]
    ]


viewMessage message =
  div [] [text message]


update msg model =
  case msg of
    Input newInput ->
      ({ model | input = newInput }, Cmd.none)
    
    Send ->
      ({ model | input = "" }, WebSocket.send echoServer model.input)
    
    NewMessage message ->
      ({ model | messages = (message :: model.messages) }, Cmd.none)


subscriptions model =
  WebSocket.listen echoServer NewMessage


messageBoxStyle =
  style
    [ ("height", "200px")
    , ("background-color", "#eee")
    , ("width", "220px")
    , ("overflow-y", "auto")
    , ("word-wrap", "break-word")
    ]
