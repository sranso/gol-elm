module Cell exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App

--Main

main =
  App.beginnerProgram
    { model = init Alive
    , update = update
    , view = view
    }

--Model

type Automaton = Dead | Alive

type alias Model = Automaton

--function def
init : Automaton -> Model
--function logic
init a = a

--Update

type Msg
  = GoToOtherSide

update : Msg -> Model -> Model
update msg model =
  case model of
    Alive -> Dead
    Dead -> Alive

--View

view : Model -> Html Msg
view model =
  let
    color =
      case model of
        Alive -> "blue"
        Dead -> "grey"
    divStyle =
      style
        [ ("background-color", color)
        , ("height", "60px")
        , ("width", "60px")
        ]
  in
    div [ divStyle, onClick GoToOtherSide ] []

