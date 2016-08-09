import Html exposing (text, div, input, span)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value, style)
import Html.App exposing (program)
import Array exposing (fromList, toList, length, get)
import Maybe exposing (withDefault)
import Random exposing (generate, int)
import String exposing (endsWith, dropRight, split)
import Time exposing (Time, second)

sentence = "This is a short sentence to type for practice."
words = fromList <| split " " sentence

type Msg
  = UpdateInputWord String
  | Tick Time

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { inputWord : String
  , wordIndex : Int
  , time : Int
  , runClock : Bool
  , wpm : Float
  }

init = (Model "" 0 0 False 0, Cmd.none)

view model =
  div []
    [ input [ onInput UpdateInputWord, value model.inputWord ] []
    , span [] [ text <| toString model.wpm ++ " wpm" ]
    , div [] <| toList <| Array.indexedMap (viewWord model) words
    , div
        [ barContainerStyle ]
        [ div 
          [ barStyle <|
              100 * (toFloat model.wordIndex / toFloat (length words)) ]
          []
        ]
    ]

viewWord model index word =
  span
    [ if index == model.wordIndex then
        highlightStyle
      else
        style []
    ]
    [ text (word ++ " ") ]

update msg model =
  case msg of
    UpdateInputWord inputWord ->
      if endsWith " " inputWord && model.inputWord == dropRight 1 inputWord then
        ( { model | inputWord = ""
          , wordIndex = model.wordIndex + 1
          , runClock = model.wordIndex < length words
          , wpm = (toFloat (String.length sentence) / (toFloat model.time / 60)) / 5
          },
          Cmd.none
        )
      else
        ( { model | inputWord = inputWord }, Cmd.none )
        
    Tick time ->
      if model.runClock then
        ( { model | time = model.time + 1 }, Cmd.none )
      else
        ( model, Cmd.none )

subscriptions model =
  Time.every second Tick

highlightStyle =
  style [ ("background-color", "yellow") ]

barContainerStyle =
  style
    [ ("width", "200px")
    , ("height", "20px")
    , ("border", "2px solid black")
    ]

barStyle percentage =
  style
    [ ("width", toString percentage ++ "%")
    , ("height", "20px")
    , ("background-color", "blue")
    ]
