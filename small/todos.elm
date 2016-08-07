import Html exposing (div, text, form, input, button, span)
import Html.Events exposing (onSubmit, onInput, onClick)
import Html.Attributes exposing (value)
import Html.App exposing (beginnerProgram)
import Array exposing (fromList, toList, push, indexedMap, slice, append, length)

main =
  beginnerProgram
    { model = model
    , view = view
    , update = update
    }

model =
  { todos = fromList []
  , text = ""
  }
  
type Msg
  = AddTodo
  | DeleteTodo Int
  | UpdateText String

view model =
  div []
    [ form [ onSubmit AddTodo ]
      [ input [ onInput UpdateText, value model.text ] []
      , button [] [ text "Add todo" ]
      ]
    , div [] (toList (indexedMap viewTodo model.todos))
    ]

viewTodo index todo =
  div []
    [ span [] [ text todo ]
    , span [ onClick (DeleteTodo index) ] [ text "✖" ]
    ]

update msg model =
  case msg of
    AddTodo ->
      { model | todos = push model.text model.todos, text = "" }
    DeleteTodo index ->
      { model | todos = 
        append 
          (slice 0 index model.todos) 
          (slice (index + 1) (length model.todos) model.todos) }
    UpdateText text ->
      { model | text = text }
