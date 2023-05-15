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
import qualified Data.Text.IO as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Collection
import Reflex.Network
import Reflex.Process
import Reflex.Vty

import System.Directory
import System.IO
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
      , (V.KEsc, [])
      ]
    let doStartStopEv = () <$ (ffilter (\(k, _) -> k == V.KEsc || k == V.KEnter) keyPress)
        doNextEv = () <$ (ffilter ((== V.KChar ' ') . fst) keyPress)
        toggleButton lblOff lblOn extraToggleEv = mdo
          active <- toggle False (leftmost [extraToggleEv, toggleEv])
          let dLabel = ffor active $ \case
                True -> lblOn
                _ -> lblOff
          toggleEv <- tile flex $ textButton def (current dLabel)
          pure active
    (active, doRemoteTranscribe, nextEv', copyClipboardEv, saveToFileEv, clearEv) <- grout (fixed 4) $ row $ do
      active <- toggleButton "Start" "Stop" doStartStopEv
      nEv <- tile flex $ textButtonStatic def "Next"
      doRemoteTranscribe <- toggleButton "Do Remote" "Do Local" never
      copyClipboardEv <- tile flex $ textButtonStatic def "Copy to clipboard"
      saveToFileEv <- tile flex $ textButtonStatic def "Save to file"
      clearEv <- tile flex $ textButtonStatic def "Clear"
      pure (active, doRemoteTranscribe, gate (current active) (leftmost [nEv, doNextEv]), copyClipboardEv, saveToFileEv, clearEv)
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
      let copyAudioEv = tag (current doRemoteTranscribe) $ _process_exit recordProc
      ((switch . current) <$>) $ networkHold (pure never) $ ffor copyAudioEv $ \case
        False -> do
          transcribeAudioProc <- createProcess (proc "./local-transcribe-audio.sh" [fileName]) $ def
          let txtEv = T.stripStart . T.decodeUtf8 <$> _process_stdout transcribeAudioProc
          pure txtEv
        True -> do
          copyAudioProc <- createProcess (proc "./copy-audio.sh" [fileName]) $ def
          let transcribeAudioEv = _process_exit copyAudioProc
          ((switch . current) <$>) $ networkHold (pure never) $ ffor transcribeAudioEv $ \_ -> do
            transcribeAudioProc <- createProcess (proc "./transcribe-audio.sh" [fileName]) $ def
            let txtEv = T.stripStart . T.decodeUtf8 <$> _process_stdout transcribeAudioProc
            pure txtEv
    (addTxtEv, addTxtAction) <- newTriggerEvent
    networkView $ ffor dResults $ \m -> forM (Map.assocs m) $ \(k, ev) -> do
      performEvent $ ffor ev $ \txt -> liftIO (addTxtAction (k, txt))

    let accumTxtFn (Left _) _ = mempty
        accumTxtFn (Right (k, v)) m = Map.singleton k v <> m
    processedTxtx <- foldDyn accumTxtFn mempty (leftmost [Right <$> addTxtEv, Left <$> clearEv])
    tile flex $ boxTitle (constant def) "Transcribed Text" $ do
      let outputTxt = current $ ffor processedTxtx $ Map.foldl (<>) ""
      networkHold blank $ ffor (tag outputTxt copyClipboardEv) $ \txt -> do
        pb <- getPostBuild
        stdinEv <- delay 0.5 (SendPipe_LastMessage (T.encodeUtf8 txt) <$ pb)
        void $ createProcess (proc "./copy-to-clipboard.sh" []) $ def { _processConfig_stdin = stdinEv }
      performEvent $ ffor (tag outputTxt saveToFileEv) $ \txt -> void $ liftIO $ do
        dir <- getCurrentDirectory
        (_, handle) <- openTempFile dir "transcript.txt"
        T.hPutStr handle txt
        hClose handle

      text outputTxt

tshow :: (Show a) => a -> Text
tshow = T.pack . show
