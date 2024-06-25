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
import           Types

import           Wuss

import           Control.Concurrent         (forkIO)
import           Control.Lens               ((.~), (^.))
import           Control.Monad              (forever, void, when)
import           Control.Monad.State        (modify)

import           Data.Aeson                 (decode)
import           Data.Function              ((&))
import           Data.IORef
import qualified Data.Text                  as T
import qualified Data.Time                  as Tm
import           Data.Time.Clock            as Clc
import qualified Data.Time.LocalTime        as Tml

import           System.IO.Unsafe

import           Network.WebSockets         (ClientApp, receiveData, sendClose, sendTextData)

import           NeatInterpolation          (text)

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

-- | Do nothing returning unit inside applicative.
pass ∷ Applicative f => f ()
pass = pure ()

extractValues ∷ OrderData -> Maybe (T.Text, T.Text)
extractValues (OrderData _ myData) =
  let bList = b myData
  in if null bList
     then Nothing
     else Just (s myData, head (head bList))

theMap ∷ BA.AttrMap
theMap = BA.attrMap V.defAttr [ (B.attrName "infoTitle", B.fg V.cyan)
                              , (B.attrName "time"     , B.fg V.yellow)
                              , (B.attrName "green"    , B.fg V.green)
                              , (B.attrName "red"      , B.fg V.red)
                              ]

-- | Defines how the brick application will work / handle events
app ∷ B.App BrickState Event Name
app =
  B.App { B.appDraw         = drawUI
        , B.appChooseCursor = B.showFirstCursor
        , B.appHandleEvent  = handleEvent
        , B.appStartEvent   = pass
        , B.appAttrMap      = const theMap
        }

btcRef  ∷ IORef Float
ethRef  ∷ IORef Float
solRef  ∷ IORef Float

btcTRef ∷ IORef Tm.LocalTime
ethTRef ∷ IORef Tm.LocalTime
solTRef ∷ IORef Tm.LocalTime

zerotime ∷ Tm.LocalTime
zerotime = Tml.utcToLocalTime Tml.utc (Clc.UTCTime (Tm.fromGregorian 1 1 1) 0)

btcRef  = unsafePerformIO $ newIORef 0.0
ethRef  = unsafePerformIO $ newIORef 0.0
solRef  = unsafePerformIO $ newIORef 0.0

btcTRef = unsafePerformIO $ newIORef zerotime
ethTRef = unsafePerformIO $ newIORef zerotime
solTRef = unsafePerformIO $ newIORef zerotime
  
ws ∷ ClientApp ()
ws connection = do
  chan <- BCh.newBChan 100

  void ∘ forkIO ∘ forever $ do
    message <- receiveData connection
    let jsonData = decode message :: Maybe OrderData
    case jsonData of
      Just dat ->
        case (extractValues dat) of
          Just (ss, p) -> do
            newTime <- getTime
            BCh.writeBChan chan $ EventUpdateTime newTime
            let chainType = 
                  case ss of
                    "BTCUSDT" -> Just (EventUpdateBTC, btcTRef, btcRef)
                    "ETHUSDT" -> Just (EventUpdateETH, ethTRef, ethRef)
                    "SOLUSDT" -> Just (EventUpdateSOL, solTRef, solRef)
                    _         -> Nothing
            case chainType of
              Just (cht, coinTRef, coinRef) -> do
                lastDiffTime <- readIORef coinTRef
                let tDiff    = Tm.diffLocalTime newTime lastDiffTime
                    tDiffSec = (round $ Tm.nominalDiffTimeToSeconds tDiff) :: Integer
                    coinNow  = (read $ T.unpack p) :: Float
                coinWas <- readIORef coinRef
                when (tDiffSec > 10) $ do
                  writeIORef coinTRef newTime
                  writeIORef coinRef coinNow
                BCh.writeBChan chan $ cht (coinNow > coinWas, p)
              Nothing  -> pass
          Nothing      -> pass
      Nothing -> pass

  t <- getTime

  -- Construct the initial state values
  let st = BrickState
            { _stTime         = t
            , _stBTC          = (False, "0") -- ^ Dan Pena
            , _stETH          = (False, "0")
            , _stSOL          = (False, "0")
            }

  let subText = [text|
{
    "req_id": "btcusdt",
    "op": "subscribe",
    "args": [
        "orderbook.1.BTCUSDT",
        "orderbook.1.ETHUSDT",
        "orderbook.1.SOLUSDT"
    ]
}
  |]

  sendTextData connection subText

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
        EventUpdateBTC btc   -> modify $ \st -> st & stBTC.~ btc
        EventUpdateETH eth   -> modify $ \st -> st & stETH.~ eth
        EventUpdateSOL sol   -> modify $ \st -> st & stSOL.~ sol

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
      vtitle "BTCUSDT:"
      <=>
      B.padLeft (B.Pad 2) (vvalue (st ^. stBTC))
      <=>
      vtitle "ETHUSDT:"
      <=>
      B.padLeft (B.Pad 2) (vvalue (st ^. stETH))
      <=>
      vtitle "SOLUSDT:"
      <=>
      B.padLeft (B.Pad 2) (vvalue (st ^. stSOL))
      <=>
      B.fill ' '

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
go _cfg = do
  runSecureClient "stream.bybit.com"
              443 "/v5/public/linear" ws
