module World exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App
import Window
import Task

import Cell

--Main
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

--Model

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

--Update

type Msg
  = NewGeneration
  | NewWindowSize Window.Size
  | SizeUpdateFailure String

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewGeneration -> makeNextGen model
    -- fxn that takes in model and returns next gen

-- define makeNextGen. write pseudo code first.
