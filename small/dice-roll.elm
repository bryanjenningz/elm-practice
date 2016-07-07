import Html exposing (div, h1, button, text, Html)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random exposing (pair, int)


main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg
  = Roll
  | NewDice (Int, Int)
  
  
type alias Model = 
  { dieFace : Int
  , dieFace2 : Int
  }


view : Model -> Html Msg
view model =
  div [centered]
    [ button [ onClick Roll ] [ text "Roll" ]
    , h1 [] [ text <| toString model.dieFace ]
    , h1 [] [ text <| toString model.dieFace2 ]
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewDice (pair (int 1 6) (int 1 6)))
    
    NewDice (newFace, newFace2) ->
      (Model newFace newFace2, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)


centered =
  style [ ("text-align", "center") ]
