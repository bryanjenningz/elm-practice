import Html exposing (div, text, form, input, button, span)
import Html.Events exposing (onSubmit, onInput, onClick)
import Html.Attributes exposing (value, style)
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
  , filter = All
  }
  
type Msg
  = AddTodo
  | DeleteTodo Int
  | ToggleTodo Int
  | UpdateText String
  | ChangeFilter Filter

type Filter = All | Active | Done

type alias Todo =
  { text : String
  , done : Bool
  }

view model =
  div []
    [ viewFilters model.filter
    , form [ onSubmit AddTodo ]
      [ input [ onInput UpdateText, value model.text ] []
      , button [] [ text "Add todo" ]
      ]
    , div [] 
      (toList (indexedMap viewTodo (filterTodos model.filter model.todos)))
    ]

filterTodos filter todos =
  case filter of
    All -> todos
    Active -> Array.filter (\todo -> not todo.done) todos
    Done -> Array.filter (\todo -> todo.done) todos

viewFilters filter =
  div []
    [ viewFilter All filter
    , viewFilter Active filter
    , viewFilter Done filter
    ]

viewFilter filterType filter =
  let
    filterText = 
      case filterType of
        All -> "All"
        Active -> "Active"
        Done -> "Done"
  in
    span
      [ if filter == filterType then activeFilter else inactiveFilter
      , onClick (ChangeFilter filterType)
      ]
      [ text filterText ]

viewTodo index todo =
  div []
    [ span
      [ if todo.done then doneStyle else (style [])
      , onClick (ToggleTodo index)
      ] [ text todo.text ]
    , span [ onClick (DeleteTodo index) ] [ text "✖" ]
    ]

update msg model =
  case msg of
    AddTodo ->
      { model | todos = push (Todo model.text False) model.todos, text = "" }
    DeleteTodo index ->
      { model | todos = 
        append 
          (slice 0 index model.todos) 
          (slice (index + 1) (length model.todos) model.todos) }
    ToggleTodo index ->
      { model | todos =
        mapIndex index toggleTodo model.todos }
    UpdateText text ->
      { model | text = text }
    ChangeFilter filter ->
      { model | filter = filter }

mapIndex index f xs =
  indexedMap (\i x -> if i == index then f x else x) xs

toggleTodo todo =
  { todo | done = not todo.done }

activeFilter =
  style [ ("color", "blue") ]

inactiveFilter =
  style []

doneStyle =
  style [ ("text-decoration", "line-through") ]
