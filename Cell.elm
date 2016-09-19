module Cell exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App

-- MAIN

main =
  App.beginnerProgram
    { model = init Alive (0, 0)
    , update = update
    , view = view
    }

-- MODEL

type LifeStatus = Dead | Alive

type alias Coords = (Int, Int)

type alias Model =
  { lifeStatus : LifeStatus
  , coords : Coords
  }

--function def
init : LifeStatus -> Coords -> Model
--function logic
init lifeStatus coords =
  { lifeStatus = lifeStatus
  , coords = coords
  }

-- UPDATE

type Msg
  = GoToOtherSide

update : Msg -> Model -> Model
update msg ({lifeStatus, coords} as model) =
  case msg of
    GoToOtherSide -> case lifeStatus of
      Alive -> { model | lifeStatus = Dead }
      Dead -> { model | lifeStatus = Alive }

-- VIEW

view : Model -> Html Msg
view ({lifeStatus, coords} as model) =
  let
    color =
      case lifeStatus of
        Alive -> "blue"
        Dead -> "grey"
    divStyle =
      style
        [ ("background-color", color)
        , ("height", "40px")
        , ("width", "40px")
        ]
  in
    div [ divStyle ] []

