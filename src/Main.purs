module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array
import Data.Tuple
import Data.Foldable (sum)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.MouseEvent as ME
import Web.Event.Event (Event, EventType(..), preventDefault, stopPropagation)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State =
  { config :: Config
  , board :: Board 
  , ranking :: Array GameRecord
  , phase :: Phase
  }

type Config =
  { boardWidth :: Int
  , boardHeight :: Int
  , numberOfBombs :: Int
  }

type Board = Array (Array Cell)

type Cell =
  { appearance :: CellAppearance
  , arroundBombs :: Int
  , hasBomb :: Boolean
  , x :: Int
  , y :: Int
  }

data CellAppearance
  = CellOpen
  | CellClose Boolean

type GameRecord =
  { score :: Int
  }

data Phase
  = Ready
  | Playing
  | GameOver
  | Clear

defaultConfig :: Config
defaultConfig =
  { boardWidth: 20
  , boardHeight: 10
  , numberOfBombs: 30
  }

makeInitialBoard :: Config -> Board
makeInitialBoard config = do
    x <- (0 .. (config.boardWidth - 1))
    pure do
       y <- (0 .. (config.boardHeight - 1))
       pure $ initialCell x y

initialCell :: Int -> Int -> Cell
initialCell x y =
  { appearance: CellClose false
  , arroundBombs: 0
  , hasBomb: false
  , x: x
  , y: y
  }

data Action
  = CellLeftClick Int Int
  | CellRightClick Int Int

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
  , phase: Ready
  }

type Screen m = H.ComponentHTML Action () m

render :: forall m. State -> Screen m
render state =
  HH.div_
    [ HH.h1_
      [ HH.text "Mine Sweeper" ]
    , HH.div_
      [ renderBoard state.board ]
    , HH.div_
      [ HH.text (case state.phase of
          Ready -> ""
          Playing -> ""
          GameOver -> "Game Over"
          Clear -> "Game Clear!!")
      ]
    ]

renderBoard :: forall m. Board -> Screen m
renderBoard board = HH.div_ do
  Tuple line x <- zip board (0 .. (length board - 1))
  pure $ HH.div_ do
     Tuple cell y <- zip line (0 .. (length line - 1))
     pure $ renderCell cell x y

onContextMenu :: forall r i. (Event -> i) -> HP.IProp (onContextMenu :: Event | r) i
onContextMenu = HE.handler (EventType "contextmenu")

renderCell :: forall m. Cell -> Int -> Int -> Screen m
renderCell cell x y = HH.button
  [ HE.onClick $ \_ -> CellLeftClick x y
  , onContextMenu $ \_ -> CellRightClick x y
  ]
  [ HH.text
  case cell.appearance of
      CellClose flag -> if flag then "@" else "~"
      CellOpen ->
        if cell.hasBomb
          then "X"
          else show cell.arroundBombs
  ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  CellLeftClick x y -> do
    state <- H.get

    let board = state.board
    newState <- do
      case state.phase of
        Playing -> pure (
          case getBoardAt x y board of
            Nothing -> state
            Just cell -> if cell.hasBomb
              then state { board = openCells x y board, phase = GameOver }
              else state { board = openCells x y board })
        Ready -> do
          boardWithBombs <- H.liftEffect $ spreadBombs state.config x y board
          let newBoard = setArroundBombNumbers boardWithBombs
          pure $ state { board = openCells x y newBoard, phase = Playing }
        _ -> pure state

    H.put newState

  CellRightClick x y -> do
     H.modify_ \state ->
       case getBoardAt x y state.board of
         Nothing -> state
         Just cell -> case cell.appearance of
           CellClose flag -> state { board = modifyBoardAt x y state.board (\cell -> cell { appearance = CellClose $ not flag }) }
           _ -> state

openOneCell :: Int -> Int -> Board -> Board
openOneCell x y board = modifyBoardAt x y board (\cell -> cell { appearance = CellOpen })

{-modifyBoardAtAll :: (Cell -> Cell) -> Board -> Array (Tuple Int Int) -> Board
modifyBoardAtAll points f board =
  foldl (\board point -> modifyBoardAt (fst point) (snd point) f board) board points
  -}
openCells :: Int -> Int -> Board -> Board
openCells x y board = 
  case getBoardAt x y board of
    Nothing -> board
    Just cell ->
      if cell.hasBomb
        then openOneCell x y board 
        else
          let
            pointsToOpen = openArroundCells
              [Tuple x y]
              (if cell.arroundBombs == 0
                then [Tuple x y]
                else [])
              board
            f :: Board -> Tuple Int Int -> Board
            f bd point = openOneCell (fst point) (snd point) bd
          in
            foldl f board pointsToOpen

openArroundCells :: Array (Tuple Int Int) -> Array (Tuple Int Int) -> Board -> Array (Tuple Int Int)
openArroundCells marked zeroCells board =
  case uncons zeroCells of
    Nothing -> marked
    Just {head: Tuple x y, tail: rest} ->
      let
        arroundPoints = [
          Tuple (x-1) (y-1), Tuple (x-1) (y), Tuple (x-1) (y+1), 
          Tuple (x) (y-1), Tuple (x) (y+1), 
          Tuple (x+1) (y-1), Tuple (x+1) (y), Tuple (x+1) (y+1)
        ]
        safePoints = flip filter arroundPoints \point ->
          case getBoardAt (fst point) (snd point) board of
            Nothing -> false
            Just cell -> not cell.hasBomb
        zeroPoints = flip filter safePoints \point ->
          case getBoardAt (fst point) (snd point) board of
            Nothing -> false
            Just cell -> cell.arroundBombs == 0
      in
        openArroundCells (marked <> (safePoints \\ marked)) (rest <> ((zeroPoints \\ marked) \\ rest)) board

generateRandomArray :: Int -> Array Int -> Effect (Array Int)
generateRandomArray _ [] = pure []
generateRandomArray 0 _  = pure []
generateRandomArray n xs = do
  index <- randomInt 0 (length xs - 1)
  let y = fromMaybe 0 (xs !! index)
      ys = fromMaybe [] (deleteAt index xs)
  cons y <$> generateRandomArray (n - 1) ys

spreadBombs :: Config -> Int -> Int -> Board -> Effect Board
spreadBombs config x y board = do
  let n = x * config.boardWidth + y
      farFromN m =
        let
          mx = m / config.boardWidth
          my = m `mod` config.boardHeight
          nx = n / config.boardWidth
          ny = n `mod` config.boardHeight
        in
          abs (mx - nx) > 1 || abs (my - ny) > 1
      indices = filter farFromN (0 .. (config.boardWidth * config.boardHeight - 1))
  ys <- generateRandomArray (config.numberOfBombs) indices
  pure $ foldl putBomb board ys
  where
    putBomb :: Board -> Int -> Board
    putBomb bd n =
      let x_ = n / config.boardWidth
          y_ = n `mod` config.boardWidth
       in modifyBoardAt x_ y_ bd $ \cell -> cell { hasBomb = true }

setArroundBombNumbers :: Board -> Board
setArroundBombNumbers board = do
  line <- board
  pure do
    cell <- line
    pure $ setNumber cell board
      where
        setNumber :: Cell -> Board -> Cell
        setNumber cell board = 
          let
            x = cell.x
            y = cell.y
            arroundCells :: Array (Maybe Cell)
            arroundCells = map (\getFunc -> getFunc board) [
              getBoardAt (x-1) (y-1), getBoardAt (x-1) (y+0), getBoardAt (x-1) (y+1),
              getBoardAt (x+0) (y-1), getBoardAt (x+0) (y+1),
              getBoardAt (x+1) (y-1), getBoardAt (x+1) (y+0), getBoardAt (x+1) (y+1)
            ]
            num = sum $ map (maybe 0 (\c -> if c.hasBomb then 1 else 0)) arroundCells
          in
            cell { arroundBombs = num }

modifyBoardAt :: Int -> Int -> Board -> (Cell -> Cell) -> Board
modifyBoardAt x y board f = fromMaybe board do
  line <- board !! x
  newLine <- modifyAt y f line
  newBoard <- updateAt x newLine board
  pure newBoard

getBoardAt :: Int -> Int -> Board -> Maybe Cell
getBoardAt x y board = do
  line <- board !! x
  cell <- line !! y
  pure cell
