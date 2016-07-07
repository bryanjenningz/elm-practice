import Html exposing (div, text, button)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import Time exposing (Time, inSeconds, inMinutes, inHours)
import String


main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { time : Time
  , isPaused : Bool
  }


type Msg
  = Tick Time
  | PauseResume


init =
  (Model 0 False, Cmd.none)


view model =
  let
    seconds =
      toString <| floor (inSeconds model.time) `rem` 60
    minutes =
      toString <| floor (inMinutes model.time) `rem` 60
    hours =
      -- +1 to turn 11 to 12, 0 to 1, ..., -8 for Pacific Standard Time
      toString <| (floor (inHours model.time) `rem` 12) + 1 - 8
  in
    div []
      [ text <| hours ++ ":" ++ padStringTime minutes ++ ":" ++ padStringTime seconds
      , button [ onClick PauseResume ] [ text (if model.isPaused then "Resume" else "Pause") ]
      ]


padStringTime stringNumber =
  if String.length stringNumber == 2 then
    stringNumber
  else
    padStringTime ("0" ++ stringNumber)


update msg model =
  case msg of
    Tick newTime ->
      (if model.isPaused then model else { model | time = newTime }, Cmd.none)
    
    PauseResume ->
      ({ model | isPaused = not model.isPaused }, Cmd.none)


subscriptions model =
  Time.every Time.second Tick
