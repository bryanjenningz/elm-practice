import Html exposing (div, h1, button, text, Html)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random exposing (pair, int)
import Svg exposing (svg, node)
import Svg.Attributes exposing (width, height, x, y, rx, ry, 
                                viewBox, cx, cy, fill, r,
                                stroke, strokeWidth)


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
    [ button [ centered, buttonStyle, onClick Roll ] [ text "Roll" ]
    , dieView model.dieFace
    , dieView model.dieFace2
    ]


rect attributes children =
    node "rect" attributes children


dieView dieFace =
  svg
    [ width "120", height "120", viewBox "0 0 120 120", centered ]
    ([ node "rect"
      [ x "10"
      , y "10"
      , width "100"
      , height "100"
      , rx "15"
      , ry "15"
      ]
      []
    ] ++ case dieFace of
      1 ->
        [ node "circle" [ fill "white", cx "60", cy "60", r "10" ] [] ]
      
      2 ->
        [ node "circle" [ fill "white", cx "40", cy "40", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "80", r "10" ] []
        ]
      
      3 ->
        [ node "circle" [ fill "white", cx "40", cy "40", r "10" ] []
        , node "circle" [ fill "white", cx "60", cy "60", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "80", r "10" ] []
        ]
      
      4 ->
        [ node "circle" [ fill "white", cx "40", cy "40", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "40", r "10" ] []
        , node "circle" [ fill "white", cx "40", cy "80", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "80", r "10" ] []
        ]
      
      5 ->
        [ node "circle" [ fill "white", cx "40", cy "40", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "40", r "10" ] []
        , node "circle" [ fill "white", cx "60", cy "60", r "10" ] []
        , node "circle" [ fill "white", cx "40", cy "80", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "80", r "10" ] []
        ]
      
      _ ->
        [ node "circle" [ fill "white", cx "40", cy "35", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "35", r "10" ] []
        , node "circle" [ fill "white", cx "40", cy "60", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "60", r "10" ] []
        , node "circle" [ fill "white", cx "40", cy "85", r "10" ] []
        , node "circle" [ fill "white", cx "80", cy "85", r "10" ] []
        ]
    )


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
  style
    [ ("text-align", "center")
    , ("display", "block")
    , ("margin", "0 auto")
    ]


buttonStyle =
  style
    [ ("width", "200px")
    , ("height", "50px")
    , ("display", "block")
    , ("background-color", "#030098")
    , ("border", "none")
    , ("border-radius", "10px")
    , ("color", "white")
    , ("font-weight", "900")
    , ("cursor", "pointer")
    ]
