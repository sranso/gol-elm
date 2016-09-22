module Cell exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App


-- MODEL


type alias Model =
  { lifeStatus : LifeStatus
  , coords : Coords
  }

type LifeStatus = Dead | Alive

type alias Coords = ( Int, Int )


init : LifeStatus -> Coords -> Model
init lifeStatus coords =
  { lifeStatus = lifeStatus
  , coords = coords
  }


-- UPDATE


type Msg
  = GoToOtherSide


update : Msg -> Model -> Model
update msg ( { lifeStatus, coords } as model ) =
  case msg of
    GoToOtherSide ->
      case lifeStatus of
        Alive -> { model | lifeStatus = Dead }
        Dead -> { model | lifeStatus = Alive }


-- VIEW


view : Model -> Html Msg
view ( { lifeStatus, coords } as model ) =
  let
    color =
      case lifeStatus of
        Alive -> "blue"
        Dead -> "grey"
    divStyle =
      style
        [ ( "background-color", color )
        , ( "height", "100%" )
        , ( "width", "100%" )
        ]
  in
    div [ divStyle ] []

