module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Functor
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Process
import Reflex.Vty

type VtyExample t m =
  ( MonadFix m
  , MonadHold t m
  , Reflex t
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocus t m
  , HasFocusReader t m
  , HasTheme t m
  )

type Manager t m =
  ( HasLayout t m
  , HasFocus t m
  )

withCtrlC :: (Monad m, HasInput t m, Reflex t) => m () -> m (Event t ())
withCtrlC f = do
  inp <- input
  f
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

darkTheme :: V.Attr
darkTheme = V.Attr {
  V.attrStyle = V.SetTo V.standout
  , V.attrForeColor = V.SetTo V.black
  , V.attrBackColor = V.Default
  , V.attrURL = V.Default
}

main :: IO ()
main = mainWidget $ withCtrlC $ do
  initManager_ $ do
    tabNavigation
    keyPress <- keyCombos $ Set.fromList
      [ (V.KEnter, [])
      , (V.KChar ' ', [])
      ]
    (active, nextEv) <- grout (fixed 4) $ row $ do
      rec
        active <- toggle False toggleStartEv
        let startLabel = ffor active $ \case
              True -> "Stop"
              _ -> "Start"
        toggleStartEv <- tile flex $ textButton def (current startLabel)
      nEv <- tile flex $ textButtonStatic def "Next"
      pure (active, leftmost [nEv, () <$ keyPress])
    tile flex $ boxTitle (constant def) "Transcribed Text" $ do
      outputDyn <- foldDyn (<>) "" $ mergeWith (<>)
        [nextEv $> "\129364"]
      text (current outputDyn)
