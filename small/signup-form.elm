import Html exposing (div, text, input, button)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, type', placeholder)
import String
import Regex exposing (regex, contains)


main =
  beginnerProgram { model = model, view = view, update = update }


type alias Model =
  { username : String
  , password : String
  , passwordAgain : String
  , age : String
  , submitted : Bool
  }


model = Model "" "" "" "" False


type Msg = Username String
         | Password String
         | PasswordAgain String
         | Age String
         | Submit


view model =
  div []
    [ input [onInput Username, placeholder "Username"] []
    , input [type' "password", onInput Password, placeholder "Password"] []
    , input [type' "password", onInput PasswordAgain, placeholder "Confirm Password"] []
    , input [onInput Age, placeholder "Age"] []
    , button [type' "submit", onClick Submit] [text "Sign Up"]
    , verifyPassword model
    ]


verifyPassword model =
  let (color, message) =
    if model.submitted then
      if ((String.length model.password) < 8) then
        ("red", "Password must be at least 8 characters long")
      else if not (contains (regex "[0-9]") model.password) ||
              not (contains (regex "[a-z]") model.password) ||
              not (contains (regex "[A-Z]") model.password) then
        ("red", "Password must contain lowercase, uppercase, and a number")
      else if model.password /= model.passwordAgain then
        ("red", "Passwords don't match")
      else if not (contains (regex "^[0-9]+$") model.age) then
        ("red", "Age must be a number")
      else
        ("green", "OK")
    else
      ("white", "")
  in
    div [style [("color", color)]] [text message]


update msg model =
  case msg of
    Username newUsername ->
      { model | username = newUsername }
    
    Password newPassword ->
      { model | password = newPassword }
    
    PasswordAgain newPasswordAgain ->
      { model | passwordAgain = newPasswordAgain }
      
    Age newAge ->
      { model | age = newAge }
    
    Submit ->
      { model | submitted = True }
