import Html exposing (div, button, text, input)
import Html.App exposing (program)
import Html.Attributes exposing (type', value, placeholder)
import Html.Events exposing (onClick)
import Time exposing (Time)
import String exposing (slice, length)


guestUsername = "Guest"
guestPassword = "Password"


main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { username : String
  , password : String
  , guestPasswordStart : Bool
  , guestPassword : Bool
  }


init =
  (Model "" "" False False, Cmd.none)


type Msg
  = UpdateUsername String
  | UpdatePassword String
  | GuestLogin
  | Tick Time


view model =
  div []
    [ input [placeholder "Username", value model.username] []
    , input [type' "password", placeholder "Password", value model.password] []
    , button [onClick GuestLogin] [text "Guest Login"]
    ]


update msg model =
  case msg of
    UpdateUsername username ->
      ({ model | username = username }, Cmd.none)
    UpdatePassword password ->
      ({ model | password = password }, Cmd.none)
    GuestLogin ->
      ({ model | guestPasswordStart = True }, Cmd.none)
    Tick time ->
      if model.guestPasswordStart then
        ({ model
          | username = ""
          , password = ""
          , guestPasswordStart = False
          , guestPassword = True }
        , Cmd.none)
      else if model.guestPassword then
        if model.username /= guestUsername then
          ({ model
            | username = slice 0 (length model.username + 1) guestUsername }
          , Cmd.none)
        else if model.password /= guestPassword then
          ({ model
            | password = slice 0 (length model.password + 1) guestPassword }
          , Cmd.none)
        else
          (model, Cmd.none)
      else
        (model, Cmd.none)


subscriptions model =
  Time.every (Time.second * 0.1) Tick
