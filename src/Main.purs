module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (cons, deleteAt, filter, foldl, length, modifyAt, null, uncons, updateAt, zip, (!!), (..), (\\))
import Data.Tuple (Tuple(..), fst, snd)
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
import Web.Event.Event (Event, EventType(..))
import Control.Alternative (guard)

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
  { boardWidth: 10
  , boardHeight: 20
  , numberOfBombs: 30
  }

makeInitialBoard :: Config -> Board
makeInitialBoard config = do
    x <- (0 .. (config.boardHeight - 1))
    pure do
       y <- (0 .. (config.boardWidth - 1))
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
  | Reset

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
render state = HH.div_
  [ HH.div_
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
  , HH.div_
      [ HH.button
        [ HE.onClick $ \_ -> Reset ]
        [ HH.text "Reset!" ]
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
            Just cell ->
              if cell.hasBomb
                then state { board = openCells x y board, phase = GameOver }
              else case cell.appearance of
                CellOpen -> openArroundCellsAtOnce x y state
                _ -> state { board = openCells x y board })
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
           CellClose flag -> state { board = modifyBoardAt x y state.board (setCellAppearance $ CellClose $ not flag) }
           _ -> state

  Reset -> do
     H.put
       { config: defaultConfig
       , board: makeInitialBoard defaultConfig
       , ranking: []
       , phase: Ready
       }

type Point = Tuple Int Int

openArroundCellsAtOnce :: Int -> Int -> State -> State
openArroundCellsAtOnce x y state = case getBoardAt x y state.board of
  Nothing -> state
  Just cell ->
    case cell.appearance of
      CellClose flag -> state
      CellOpen ->
        let
          arroundPoints = getArroundPoints x y
          numberOfFlagsArround = length $ filterPointsOnBoard isFlagCell state.board arroundPoints
          isFlagCell c = case c.appearance of
            CellClose true -> true
            _ -> false
        in
          if cell.arroundBombs == numberOfFlagsArround
            then openAllClosedNoFlagCellsArround x y state
            else state

openAllClosedNoFlagCellsArround :: Int -> Int -> State -> State
openAllClosedNoFlagCellsArround x y state =
  let
    arroundPoints = getArroundPoints x y
    pointsToOpen = filterPointsOnBoard noFlagClosePoint state.board arroundPoints
    noFlagClosePoint cell = case cell.appearance of
      CellClose false -> true
      _ -> false
    newBoard = modifyBoardAtPoints (setCellAppearance CellOpen) state.board pointsToOpen
    bombPoints = filterPointsOnBoard (_.hasBomb) state.board pointsToOpen
  in
    if null bombPoints
      then state { board = newBoard }
      else state { board = newBoard, phase = GameOver }

openCells :: Int -> Int -> Board -> Board
openCells x y board = 
  case getBoardAt x y board of
    Nothing -> board
    Just cell ->
      if cell.hasBomb
        then modifyBoardAt x y board (setCellAppearance CellOpen)
        else
          let
            pointsToOpen = openArroundCells [Tuple x y] zeroCells board
            zeroCells =
              if cell.arroundBombs == 0
                then [Tuple x y]
                else []
          in
            modifyBoardAtPoints (setCellAppearance CellOpen) board pointsToOpen

openArroundCells :: Array (Tuple Int Int) -> Array (Tuple Int Int) -> Board -> Array (Tuple Int Int)
openArroundCells marked zeroCells board =
  case uncons zeroCells of
    Nothing -> marked
    Just {head: Tuple x y, tail: rest} ->
      let
        arroundPoints = getArroundPoints x y
        safePoints = filterPointsOnBoard (not _.hasBomb) board arroundPoints
        zeroPoints = filterPointsOnBoard (\cell -> cell.arroundBombs == 0) board safePoints
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
          my = m `mod` config.boardWidth
        in
          abs (mx - x) > 1 || abs (my - y) > 1
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
setArroundBombNumbers board =
  modifyBoardAtAll setNumber board
    where
      setNumber :: Cell -> Cell
      setNumber cell = 
        let
          arroundPoints = getArroundPoints cell.x cell.y
          pointsHasBomb = filterPointsOnBoard (_.hasBomb) board arroundPoints
        in
          cell { arroundBombs = length pointsHasBomb }

-- Helper Functions --

modifyBoardAt :: Int -> Int -> Board -> (Cell -> Cell) -> Board
modifyBoardAt x y board f = fromMaybe board do
  line <- board !! x
  newLine <- modifyAt y f line
  newBoard <- updateAt x newLine board
  pure newBoard

modifyBoardAtPoints :: (Cell -> Cell) -> Board -> Array Point -> Board
modifyBoardAtPoints modifyCell board points = foldl f board points
  where
    f :: Board -> Point -> Board
    f bd point = modifyBoardAt (fst point) (snd point) bd modifyCell


modifyBoardAtAll :: (Cell -> Cell) -> Board -> Board
modifyBoardAtAll modifyCell board = modifyBoardAtPoints modifyCell board points
  where
    xlen = length board
    ylen = length $ fromMaybe [] (board !! 0)
    points = do
       x <- (0 .. (xlen - 1))
       y <- (0 .. (ylen - 1))
       pure $ Tuple x y

getBoardAt :: Int -> Int -> Board -> Maybe Cell
getBoardAt x y board = do
  line <- board !! x
  cell <- line !! y
  pure cell

getArroundPoints :: Int -> Int -> Array Point
getArroundPoints x y = do
  dx <- (-1 .. 1)
  dy <- (-1 .. 1)
  guard $ dx /= 0 || dy /= 0
  pure $ Tuple (x + dx) (y + dy)

filterPointsOnBoard :: (Cell -> Boolean) -> Board -> Array Point -> Array Point
filterPointsOnBoard pred board points = filter f points
  where
    f (Tuple x y) = case getBoardAt x y board of
      Nothing -> false
      Just cell -> pred cell

setCellAppearance :: CellAppearance -> Cell -> Cell
setCellAppearance appearance cell = cell { appearance = appearance }
