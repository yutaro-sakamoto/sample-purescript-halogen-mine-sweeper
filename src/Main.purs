module Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
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
  }

type Config =
  { boardWidth :: Int
  , boardHeight :: Int
  , numberOfBombs :: Int
  }

defaultConfig :: Config
defaultConfig =
  { boardWidth: 20
  , boardHeight: 10
  , numberOfBombs: 30
  }

type Board =
  { matrix :: Int
  }
initialBoard :: Board
initialBoard = {
  matrix: 0
  }

type GameRecord =
  { score :: Int
  }

data Action = DoNothing

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
  , board: initialBoard
  , ranking: []
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h1_
        [ HH.text "Mine Sweeper" ]
    , HH.p_
        [ HH.text "Hello world" ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  _ -> do
     H.modify_ \x -> x
