import Html exposing (div, button, text)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)

main =
  beginnerProgram {model = 0, view = view, update = update}

type Msg = Increment | Decrement | Reset

view model =
  div [] 
    [ button [ onClick Increment ] [ text "+" ]
    , div [] [ text <| toString model ]
    , button [ onClick Decrement ] [ text "-" ]
    , div []
      [ button [ onClick Reset ] [ text "Reset" ] ]
    ]


update msg model =
  case msg of
    Increment ->
      model + 1
    
    Decrement ->
      max (model - 1) 0
      
    Reset ->
      0
