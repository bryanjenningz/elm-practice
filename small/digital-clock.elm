import Html exposing (div, text)
import Html.App exposing (program)
import Time exposing (Time, inSeconds, inMinutes, inHours)
import String


main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model = Time


type Msg
  = Tick Time


init =
  (0, Cmd.none)


view model =
  let
    seconds =
      toString <| floor (inSeconds model) `rem` 60
    minutes =
      toString <| floor (inMinutes model) `rem` 60
    hours =
      -- +1 to turn 11 to 12, 0 to 1, ..., -8 for Pacific Standard Time
      toString <| (floor (inHours model) `rem` 12) + 1 - 8
  in
    div []
      [ text <| hours ++ ":" ++ padStringTime minutes ++ ":" ++ padStringTime seconds ]


padStringTime stringNumber =
  if String.length stringNumber == 2 then
    stringNumber
  else
    padStringTime ("0" ++ stringNumber)


update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)


subscriptions model =
  Time.every Time.second Tick
