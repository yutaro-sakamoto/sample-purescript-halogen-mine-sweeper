module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array
import Data.Tuple
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State =
  { config :: Config
  , board :: Board 
  , ranking :: Array GameRecord
  , start :: Boolean
  }

type Config =
  { boardWidth :: Int
  , boardHeight :: Int
  , numberOfBombs :: Int
  }

type Board = Array (Array Cell)

type Cell =
  { appearance :: CellAppearance
  , arroundBomb :: Int
  , hasBomb :: Boolean
  }

data CellAppearance
  = CellOpen
  | CellClose

type GameRecord =
  { score :: Int
  }

defaultConfig :: Config
defaultConfig =
  { boardWidth: 20
  , boardHeight: 10
  , numberOfBombs: 30
  }

makeInitialBoard :: Config -> Board
makeInitialBoard config =
  replicate config.boardWidth $ replicate config.boardHeight initialCell

initialCell :: Cell
initialCell =
  { appearance: CellClose
  , arroundBomb: 0
  , hasBomb: false
  }

data Action
  = CellClick Int Int

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { config: defaultConfig
  , board: makeInitialBoard defaultConfig
  , ranking: []
  , start: false
  }

type Screen m = H.ComponentHTML Action () m

render :: forall m. State -> Screen m
render state =
  HH.div_
    [ HH.h1_
        [ HH.text "Mine Sweeper" ]
    , HH.div_
        [ renderBoard state.board ]
    ]

renderBoard :: forall m. Board -> Screen m
renderBoard board = HH.div_ do
  Tuple line x <- zip board (0 .. (length board - 1))
  pure $ HH.div_ do
     Tuple cell y <- zip line (0 .. (length line - 1))
     pure $ renderCell cell x y 

renderCell :: forall m. Cell -> Int -> Int -> Screen m
renderCell cell x y = HH.span
  [ HE.onClick $ \_ -> CellClick x y ]
  [ HH.text
    case cell.appearance of
      CellClose -> "O"
      CellOpen ->
        if cell.hasBomb
          then "x"
          else show $ cell.arroundBomb
  ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  CellClick x y ->
    H.modify_ \state ->
      if state.start
        then state
        else state { board = fromMaybe state.board $ updateBoard state.board x y }

updateBoard :: Board -> Int -> Int -> Maybe Board
updateBoard board x y = do
  line <- board !! x
  cell <- line !! y
  let newCell = cell { appearance = CellOpen, hasBomb = true }
  newLine <- updateAt y newCell line
  newBoard <- updateAt x newLine board
  pure newBoard
