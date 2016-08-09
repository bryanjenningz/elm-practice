import Html exposing (text, div, input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Html.App exposing (program)
import Array exposing (fromList, length, get)
import Maybe exposing (withDefault)
import Random exposing (generate, int)
import String exposing (endsWith)
import Time exposing (Time, second)

words = fromList ["apple", "banana", "carrot", "date", "edemame", "fig"]

type Msg
  = NewWordIndex Int
  | UpdateInputWord String
  | Tick Time

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { charCount : Int
  , inputWord : String
  , word : String
  , timeLeft : Int
  , runClock : Bool
  }

init = (Model 0 "" "" 60 True, generate NewWordIndex (int 0 (length words - 1)))

view model =
  div []
    [ input [ onInput UpdateInputWord, value model.inputWord ] []
    , div [] [ text model.word ]
    , div [] [ text (if model.word == model.inputWord then "OK" else "") ]
    , div [] [ text (toString model.timeLeft) ]
    , div [] [ text ( if not model.runClock
                      then (toString <| model.charCount // 5) ++ " WPM"
                      else "" )
             ]
    ]

update msg model =
  case msg of
    NewWordIndex index->
      ({ model | word = (withDefault "zucchini" (get index words)) }, Cmd.none)
    UpdateInputWord inputWord ->
      if endsWith " " inputWord then
        ( { model | inputWord = ""
          , charCount =
              if model.inputWord == model.word then
                model.charCount + String.length model.word
              else
                model.charCount
          } 
        , generate NewWordIndex (int 0 (length words))
        )
      else
        ({ model | inputWord = inputWord }, Cmd.none)
    Tick time ->
      if model.runClock then
        ( { model | timeLeft = model.timeLeft - 1
          , runClock = model.timeLeft > 1
          }
        , Cmd.none)
      else
        (model, Cmd.none)

subscriptions model =
  Time.every second Tick
