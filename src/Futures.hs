{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  #-}

module Futures
  ( go
  ) where

import           Prelude.Unicode

import           Bricks
import           Config
import           Ticker
import           Types
import           Utils

import           Wuss

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception          (SomeException, catch)
import           Control.Lens               ((.~), (^.))
import           Control.Monad              (forever, void, when)
import           Control.Monad.State        (modify)

import           Data.Aeson                 (decode)
import           Data.Function              ((&))
import           Data.IORef
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Time                  as Tm

import           System.IO.Unsafe

import           Network.WebSockets         (ClientApp, receiveData, sendClose, sendTextData)

import           Brick                      ((<=>))
import qualified Brick                      as B
import qualified Brick.AttrMap              as BA
import qualified Brick.BChan                as BCh
import qualified Brick.Widgets.Border       as BB
import qualified Brick.Widgets.Border.Style as BBS

import qualified Graphics.Vty               as V
import qualified Graphics.Vty.Config
import qualified Graphics.Vty.CrossPlatform
import qualified Graphics.Vty.Input.Events  as K

theMap ∷ BA.AttrMap
theMap = BA.attrMap V.defAttr [ (B.attrName "infoTitle", B.fg V.cyan)
                              , (B.attrName "time"     , B.fg V.yellow)
                              , (B.attrName "green"    , B.fg V.green)
                              , (B.attrName "red"      , B.fg V.red)
                              ]

app ∷ B.App BrickState Event Name
app =
  B.App { B.appDraw         = drawUI
        , B.appChooseCursor = B.showFirstCursor
        , B.appHandleEvent  = handleEvent
        , B.appStartEvent   = pass
        , B.appAttrMap      = const theMap
        }

coinRefs ∷ IORef (M.Map String (Float, Tm.LocalTime))
coinRefs = unsafePerformIO $ newIORef M.empty

ws ∷ ClientApp ()
ws connection = do
  chan <- BCh.newBChan 100

  tickerUSDTs <- extractTickerUSDTs
  writeIORef coinRefs (M.fromList $ map (\x -> (x, (0.0, zerotime))) tickerUSDTs)

  void ∘ forkIO ∘ forever $ do
    message <- receiveData connection
    let jsonData = decode message :: Maybe OrderData
    case jsonData of
      Just dat ->
        case (extractValues dat) of
          Just (tt, p) -> do
            let ss = T.unpack tt
            newTime   <- getTime
            mcoinRefs <- readIORef coinRefs
            BCh.writeBChan chan $ EventUpdateTime newTime

            case M.lookup ss mcoinRefs of
              Just (coinWas, lastDiffTime) -> do
                let tDiff    = Tm.diffLocalTime newTime lastDiffTime
                    tDiffSec = (round $ Tm.nominalDiffTimeToSeconds tDiff) :: Integer
                    coinNow  = (read $ T.unpack p) :: Float
                when (tDiffSec > 2) $
                  writeIORef coinRefs $ M.insert ss (coinNow, newTime) mcoinRefs
                BCh.writeBChan chan (EventUpdateCoin $ (ss, (coinNow > coinWas, p)))
              Nothing  -> pass
          Nothing      -> pass
      Nothing -> pass

  t <- getTime

  -- Construct the initial state values
  let st = BrickState
            { _stTime         = t
            , _stCoin         = M.empty
            }

  ts <- getTickerAsString
  sendTextData connection $ T.pack ts

  -- And run brick
  let vtyBuilder = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- vtyBuilder

  void $ B.customMain initialVty vtyBuilder (Just chan) app st

  sendClose connection (T.pack "connection closed")
 where
  -- | Get the local time
  getTime = do
    t   <- Tm.getCurrentTime
    tz  <- Tm.getCurrentTimeZone
    pure $ Tm.utcToLocalTime tz t

handleEvent ∷ B.BrickEvent Name Event -> B.EventM Name BrickState ()
handleEvent ev =
  case ev of
    (B.VtyEvent _ve@(V.EvKey k ms)) ->
      case (k, ms) of
        (K.KChar 'q', []) -> B.halt  -- Exit on "q" key event
        (K.KEsc, [])      -> B.halt
        _                 -> pass

    (B.AppEvent event) ->
      case event of
        EventUpdateTime time -> modify $ \st -> st & stTime.~ time
        EventUpdateCoin coin -> modify $ \st -> st & stCoin.~ M.insert (fst coin) (snd coin) (_stCoin st)

    _ -> pass

drawUI ∷ BrickState -> [B.Widget Name]
drawUI st =
  [B.padAll 1 contentBlock]

  where
    contentBlock =
      (B.withBorderStyle BBS.unicode $ BB.border timeBlock)
      <=>
      B.padTop (B.Pad 1) resultsBlock

    resultsBlock =
      ( B.withAttr (B.attrName "infoTitle") $ B.txt "Derivatives " )
      <=>
      ( B.padTop (B.Pad 1) $ resultsDetail )
    
    resultsDetail =
      B.padLeft (B.Pad 1) $
      B.hLimit 60 $
      foldr (\key acc -> 
        let (green, value) = (st ^. stCoin) M.! key
        in vtitle (T.pack (key ++ ":"))
          <=>
          B.padLeft (B.Pad 2) (vvalue (green, value))
          <=>
          acc
      ) (B.fill ' ') (M.keys (st ^. stCoin))

    timeBlock = time (st ^. stTime)

    vvalue ∷ (Bool, T.Text) -> B.Widget n
    vvalue (green, t) =
      let attrName = if green then "green" else "red"
      in B.withAttr (B.attrName attrName) $ B.txt t

    vtitle t =
      B.withAttr (B.attrName "infoTitle") $
      B.txt t

    time t =
      B.padLeft (B.Pad 1) $
      B.padRight (B.Pad 1) $
      B.hLimit 20 $
      B.withAttr (B.attrName "time") $
      B.str (Tm.formatTime Tm.defaultTimeLocale "%H:%M:%S" t)

go ∷ Conf -> IO ()
go _cfg = runSecureClientLoop
 where
  runSecureClientLoop =
    runSecureClient "stream.bybit.com"
                443 "/v5/public/linear" ws `catch` handleException
  handleException ∷ SomeException -> IO ()
  handleException e = do
    putStrLn $ "Error: " ++ show e
    putStrLn "Reconnecting..."
    threadDelay 1000000
    runSecureClientLoop
