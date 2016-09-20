module World exposing (..)

import Html exposing (Html, div, table, tr, td, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App
import Window
import Task
import Random
import Debug
import Cell exposing (..)

-- TODO LIST
-- figure out why some cells aren't going to the other side
-- add random start
-- add styles to button
-- remove extra whitespace for world / fix resizing

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

init : (Model, Cmd Msg)
init =
  let
    listOfCells = List.repeat 10 (List.repeat 10 0)
    newEcosystem = List.indexedMap (\i row -> row |> List.indexedMap (\j num -> (Cell.init Cell.Alive (i, j)))) listOfCells
    size = { width = 800, height = 800 }
    model = { ecosystem = newEcosystem, windowSize = size, generations = 0 }
    windowSizeCmd = getWindowSize
    cmds = Cmd.batch [windowSizeCmd]
  in
    (model, cmds)

getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size

neighbors : Coords -> List Coords
-- counter clockwise from top left where i is row, j is col
neighbors (i, j) = [(i + 1, j - 1),
                    (i + 1, j),
                    (i + 1, j + 1),
                    (i, j + 1),
                    (i - 1, j + 1),
                    (i - 1, j),
                    (i - 1, j - 1),
                    (i, j - 1)]

-- index map goes here

-- UPDATE

type Msg
  = CellMessage Cell.Msg
  | NewWindowSize Window.Size
  | SizeUpdateFailure String
  | NextGeneration
  | NewEcosystem Ecosystem

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CellMessage cellMsg -> (model, Cmd.none)
    NewWindowSize newWindowSize -> ({ model | windowSize = newWindowSize }, Cmd.none)
    SizeUpdateFailure _ -> (model, Cmd.none)
    NextGeneration -> (makeNextGen model, Cmd.none)
    NewEcosystem newEcosystem -> ({ model | ecosystem = newEcosystem, generations = 0 }, Cmd.none)

-- define makeNextGen. write pseudo code first.
makeNextGen : Model -> Model
makeNextGen ({ecosystem, windowSize, generations} as model) =
  let
    newGen = List.map (\row -> List.map (\cellModel -> liveOrDie cellModel ecosystem) row) ecosystem
  in
    { model
      | ecosystem = newGen
    }

liveOrDie : Cell.Model -> Ecosystem -> Cell.Model
liveOrDie ({lifeStatus, coords} as model) ecosystem =
  let
    cellNeighbors = neighbors coords
    isNeighborAndAlive = (\cellModel -> 
      if ((List.member (.coords cellModel) cellNeighbors) && (cellModel.lifeStatus == Alive)) then
        True
      else
        False
      )
    neighbs = Debug.log "live ne " (List.concatMap (\row -> List.filter isNeighborAndAlive row) ecosystem)
    liveNeighbors = List.length (List.concatMap (\row -> List.filter isNeighborAndAlive row) ecosystem)
  in
    case lifeStatus of
      Alive -> case liveNeighbors of
        2 -> model
        3 -> model
        _ -> Cell.update GoToOtherSide model
      Dead -> case liveNeighbors of
        3 -> Cell.update GoToOtherSide model
        _ -> model

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
      rows = List.map
        (\row -> tr [] (row |>
          List.map (\cellModel ->
            td [ cellStyle ] [ (renderCell cellModel) ]))
        )
        model.ecosystem
      cellTable = table [] rows
      mainDivStyle = style [ ("width", size ++ "px") ]
  in
      div [ mainDivStyle ]
          [ div [ style [ ("flex-grow", "100") ] ] [ cellTable ],
            button [ onClick NextGeneration ] [ text "Next gen!" ]
          ]

renderCell : Cell.Model -> Html Msg
renderCell cellModel =
  cellModel
    |> Cell.view
    |> App.map CellMessage


