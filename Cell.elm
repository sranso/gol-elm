module Cell exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App

-- MAIN

main =
  App.beginnerProgram
    { model = init Alive
    , update = update
    , view = view
    }

-- MODEL

type Automaton = Dead | Alive

type alias Model = Automaton

--function def
init : Automaton -> Model
--function logic
init a = a

-- UPDATE

type Msg
  = GoToOtherSide

update : Msg -> Model -> Model
update msg model =
  case model of
    Alive -> Dead
    Dead -> Alive

-- VIEW

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

