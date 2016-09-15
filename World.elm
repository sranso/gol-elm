module World exposing (..)

import Html exposing (Html, div, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App
import Window
import Task
import Cell exposing (..)

-- MAIN
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { ecosystem : Ecosystem
  , windowSize : Window.Size
  , generations : Int
  }

type alias Ecosystem = List (List Cell.Model)

type alias Coords = (Int, Int)

init : (Model, Cmd Msg)
init =
  let
    newEcosystem = List.repeat 10 (List.repeat 10 (Cell.init Cell.Alive))
    size = { width = 800, height = 800 }
    model = { ecosystem = newEcosystem, windowSize = size, generations = 0 }
    windowSizeCmd = getWindowSize
    cmds = Cmd.batch [windowSizeCmd]
  in
    (model, cmds)

getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size

neighbors : Coords -> List Coords
neighbors (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

-- index map goes here

-- UPDATE

type Msg
  = NextGeneration Cell.Msg
  | NewWindowSize Window.Size
  | SizeUpdateFailure String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NextGeneration cellMsg -> (model, Cmd.none)
    NewWindowSize newWindowSize -> ({ model | windowSize = newWindowSize }, Cmd.none)
    SizeUpdateFailure _ -> (model, Cmd.none)

-- define makeNextGen. write pseudo code first.

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes NewWindowSize

-- VIEW

view : Model -> Html Msg
view ({ecosystem, windowSize, generations} as model) =
  let
      minSize = (Basics.min windowSize.width windowSize.height) |> toFloat
      size = toString minSize
      cellSize = toString (minSize / 10)
      cellStyle =
        style
          [ ("width", cellSize ++ "px")
          , ("height", cellSize ++ "px")
          ]
      rows = List.indexedMap
        (\i row -> tr [] (row |>
          List.indexedMap (\j cellModel ->
            td [ cellStyle ] [ (renderCell cellModel) ]))
        )
        model.ecosystem
      automatonTable = table [] rows
      mainDivStyle = style [ ("width", size ++ "px") ]
  in
      div [ mainDivStyle ]
          [ div [ style [ ("flex-grow", "100") ] ] [ automatonTable ] ]

renderCell : Cell.Model -> Html Msg
renderCell cellModel =
  cellModel
    |> Cell.view
    |> App.map NextGeneration




