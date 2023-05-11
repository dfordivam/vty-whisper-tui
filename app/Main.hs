module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Collection
import Reflex.Network
import Reflex.Process
import Reflex.Vty
import System.IO.Temp
import System.Posix.Signals
import System.Process hiding (createProcess)

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
    (active, nextEv', copyClipboardEv) <- grout (fixed 4) $ row $ do
      rec
        active <- toggle False toggleStartEv
        let startLabel = ffor active $ \case
              True -> "Stop"
              _ -> "Start"
        toggleStartEv <- tile flex $ textButton def (current startLabel)
      nEv <- tile flex $ textButtonStatic def "Next"
      copyClipboardEv <- tile flex $ textButtonStatic def "Copy to clipboard"
      pure (active, gate (current active) (leftmost [nEv, () <$ keyPress]), copyClipboardEv)
    let stopEv = fforMaybe (updated active) $ \case
          False -> Just ()
          _ -> Nothing
    let startEv = fforMaybe (updated active) $ \case
          True -> Just ()
          _ -> Nothing
    nextEv :: Event t (Int, ()) <- numberOccurrences (leftmost [startEv, nextEv'])
    let addNEv = ((\(k, _) -> Map.singleton k (Just ())) <$> nextEv)
    dResults <- listHoldWithKey mempty addNEv $ \k _ -> do
      fileName <- liftIO $ emptySystemTempFile "audio-"
      stopRecordEv <- headE (keyboardSignal <$ leftmost [nextEv', stopEv])
      recordProc <- createProcess (proc "./record-audio.sh" [fileName]) $ def { _processConfig_signal = stopRecordEv }
      let copyAudioEv = _process_exit recordProc
      ((switch . current) <$>) $ networkHold (pure never) $ ffor copyAudioEv $ \_ -> do
        copyAudioProc <- createProcess (proc "./copy-audio.sh" [fileName]) $ def
        let transcribeAudioEv = _process_exit copyAudioProc
        ((switch . current) <$>) $ networkHold (pure never) $ ffor transcribeAudioEv $ \_ -> do
          transcribeAudioProc <- createProcess (proc "./transcribe-audio.sh" [fileName]) $ def
          let txtEv = T.decodeUtf8 <$> _process_stdout transcribeAudioProc
          pure txtEv
    (addTxtEv, addTxtAction) <- newTriggerEvent
    networkView $ ffor dResults $ \m -> forM (Map.assocs m) $ \(k, ev) -> do
      performEvent $ ffor ev $ \txt -> liftIO (addTxtAction (k, txt))
    processedTxtx <- foldDyn (<>) mempty (uncurry Map.singleton <$> addTxtEv)
    tile flex $ boxTitle (constant def) "Transcribed Text" $ do
      let outputTxt = current $ (snd <$>) $ ffor processedTxtx $ Map.foldlWithKey (\(pk, pt) k t -> if k == (pk + 1) then (k, pt <> t) else (pk, pt)) (-1, "")
--       performEvent $ ffor (tag outputTxt copyClipboardEv) $ \t -> do
-- processConfig_stdin
--         liftIO $ callProcess "./copy-to-clipboard.sh"
      text outputTxt

tshow :: (Show a) => a -> Text
tshow = T.pack . show
