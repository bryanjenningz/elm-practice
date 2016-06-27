import Html exposing (li, text, ul)
import Html.Attributes exposing (class)

foods = ["Apples", "Bananas", "Cabbage"]
foodHtml food = li [class "food-item"] [text food]

main =
  ul [class "grocery-list"]
    (List.map foodHtml foods)
