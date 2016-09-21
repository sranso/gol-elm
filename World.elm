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
  , leftToRandomize : Int
  }

type alias Ecosystem = List (List Cell.Model)


rows =
  10

columns =
  10


init : ( Model, Cmd Msg )
init =
  let
    listOfCells = List.repeat columns ( List.repeat rows True )
    newEcosystem = makeNewEcosystem listOfCells
    size =
      { width = 800
      , height = 800
      }
    model =
      { ecosystem = newEcosystem
      , windowSize = size
      , generations = 0
      , leftToRandomize = 0
      }
    windowSizeCmd = getWindowSize
    cmds = Cmd.batch [ windowSizeCmd ]
  in
    ( model, cmds )


makeNewEcosystem : List ( List Bool ) -> Ecosystem
makeNewEcosystem ecosystem =
  List.indexedMap (\ i row ->
    row |> List.indexedMap (\j num ->
      ( Cell.init Cell.Alive ( i, j ) )
    )
  ) ecosystem

getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size

neighbors : Coords -> List Coords
-- Neighbors are ordered counter clockwise from top left where i is row, j is col
neighbors ( i, j ) = [ ( i + 1, j - 1 )
                     , ( i + 1, j )
                     , ( i + 1, j + 1 )
                     , ( i, j + 1 )
                     , ( i - 1, j + 1 )
                     , ( i - 1, j )
                     , ( i - 1, j - 1 )
                     , ( i, j - 1 )
                    ]


-- UPDATE


type Msg
  = CellMessage Cell.Msg
  | NewWindowSize Window.Size
  | SizeUpdateFailure String
  | NextGeneration
  | NewEcosystem Ecosystem
  | RandomizeEcosystem
  | ChangeNextCellTo Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CellMessage cellMsg -> ( model, Cmd.none )
    NewWindowSize newWindowSize -> ( { model | windowSize = newWindowSize }, Cmd.none )
    SizeUpdateFailure _ -> ( model, Cmd.none )
    NextGeneration -> ( makeNextGen model, Cmd.none )
    NewEcosystem newEcosystem ->
      ( { model | ecosystem = newEcosystem, generations = 0 }
        , Cmd.none
      )
    RandomizeEcosystem ->
      ( { model | leftToRandomize = rows * columns - 1 }
        , Random.generate ChangeNextCellTo Random.bool
      )
    ChangeNextCellTo on ->
      let
          row =
            model.leftToRandomize // rows
          col =
            model.leftToRandomize % columns
      in
        ( { model
            | ecosystem = ecoWithSpot row col on model.ecosystem
            , leftToRandomize = ( model.leftToRandomize - 1 )
          }
          , if model.leftToRandomize > 0 then
            Random.generate ChangeNextCellTo Random.bool
          else
            Cmd.none
        )

makeNextGen : Model -> Model
makeNextGen ( { ecosystem, windowSize, generations } as model ) =
  let
    newGen =
      List.map (\ row ->
        List.map (\ cellModel ->
          liveOrDie cellModel ecosystem
        ) row
      ) ecosystem
  in
    { model
      | ecosystem = newGen
    }

boolToLifeStatus : Bool -> LifeStatus
boolToLifeStatus on =
  if on then
    Alive
  else
    Dead

ecoWithSpot : Int -> Int -> Bool -> Ecosystem -> Ecosystem
ecoWithSpot row col on ecosystem =
  let
    lifeStatus = boolToLifeStatus on
  in
    case ecosystem of
      [] ->
        []
      firstRow :: restOfRows ->
        if row > 0 then
          firstRow :: ( ecoWithSpot (row - 1) col on restOfRows )
        else
          case firstRow of
            [] ->
              [] :: restOfRows
            firstCell :: restOfCells ->
              if col > 0 then
                case ecoWithSpot row ( col - 1 ) on ( restOfCells :: restOfRows ) of
                  [] ->
                    []
                  changedRow :: _ ->
                    ( firstCell :: changedRow ) :: restOfRows
               else
                ( { firstCell | lifeStatus = lifeStatus } :: restOfCells ) :: restOfRows

liveOrDie : Cell.Model -> Ecosystem -> Cell.Model
liveOrDie ( { lifeStatus, coords } as model ) ecosystem =
  let
    cellNeighbors = neighbors coords
    isNeighborAndAlive = (\ cellModel -> 
      if ( ( List.member ( .coords cellModel ) cellNeighbors )
           && ( cellModel.lifeStatus == Alive ) ) then
        True
      else
        False
      )
    liveNeighbors =
      List.length ( List.concatMap (\ row ->
        List.filter isNeighborAndAlive row ) ecosystem
      )
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
view ( { ecosystem, windowSize, generations } as model ) =
  let
      minSize = ( Basics.min windowSize.width windowSize.height ) |> toFloat
      size = toString minSize
      cellSize = toString ( minSize / 10 )
      cellStyle =
        style
          [ ("width", cellSize ++ "px")
          , ("height", cellSize ++ "px")
          ]
      rows = List.map (\ row ->
        tr [] ( row
          |> List.map (\ cellModel ->
            td [ cellStyle ] [ (renderCell cellModel) ] )
          )
        ) model.ecosystem
      cellTable = table [] rows
      mainDivStyle = style [ ("width", size ++ "px") ]
  in
      div [ mainDivStyle ]
          [ div [ style [ ( "flex-grow", "100" ) ] ] [ cellTable ]
          , button [ onClick NextGeneration ] [ text "Next gen!" ]
          , button [ onClick RandomizeEcosystem ] [ text "New random!" ]
          ]

renderCell : Cell.Model -> Html Msg
renderCell cellModel =
  cellModel
    |> Cell.view
    |> App.map CellMessage

