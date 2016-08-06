import Html exposing (div, text, button, span)
import Html.Events exposing (onClick)
import Html.App exposing (beginnerProgram)
import Array exposing (indexedMap, fromList, slice, get,
                       toList, fromList, push, append, length)
import Maybe exposing (withDefault)

main =
  beginnerProgram
    { model = (fromList [])
    , view = view
    , update = update
    }

type Msg
  = AddCounter
  | RemoveCounter Int
  | Increment Int
  | Decrement Int

view model =
  div []
    [ button [ onClick AddCounter ] [ text "Add Counter" ]
    , div [] (toList (indexedMap viewCounter model))
    ]

viewCounter index count =
  div []
    [ button [ onClick (Decrement index) ] [ text "-" ]
    , span [] [ text (toString count) ]
    , button [ onClick (Increment index) ] [ text "+" ]
    , button [ onClick (RemoveCounter index) ] [ text "✖" ]
    ]

mapNth n f array =
  indexedMap (\i x -> if i == n then f x else x) array

update msg model =
  case msg of
    AddCounter ->
      push 0 model
    RemoveCounter index ->
      append (slice 0 index model) (slice (index + 1) (length model) model)
    Increment index ->
      mapNth index (\x -> x + 1) model
    Decrement index ->
      mapNth index (\x -> x - 1) model
