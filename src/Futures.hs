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
import           Control.Monad              (forever, void)
import           Control.Monad.State        (modify)

import           Data.Aeson                 (decode)
import           Data.Function              ((&))
import qualified Data.Text                  as T
import qualified Data.Time                  as Tm

import           Network.WebSockets         (ClientApp, receiveData, sendClose, sendTextData)

import           NeatInterpolation          (text)

import           Brick                      ((<=>))
import qualified Brick                      as B
import qualified Brick.AttrMap              as BA
import qualified Brick.BChan                as BCh
import qualified Brick.Widgets.Border       as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Edit         as BE
import qualified Brick.Widgets.List         as BL

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
theMap = BA.attrMap V.defAttr [ (BE.editAttr           , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr    , V.black `B.on` V.yellow)
                              , (BL.listAttr           , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr   , V.blue `B.on` V.white)
                              , (B.attrName "infoTitle", B.fg V.cyan)
                              , (B.attrName "time"     , B.fg V.yellow)
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
            t <- getTime
            BCh.writeBChan chan $ EventUpdateTime t
            case ss of
              "BTCUSDT" -> BCh.writeBChan chan $ EventUpdateBTC p
              "ETHUSDT" -> BCh.writeBChan chan $ EventUpdateETH p
              "SOLUSDT" -> BCh.writeBChan chan $ EventUpdateSOL p
              _         -> pass
          Nothing      -> pass
      Nothing -> pass

  t <- getTime

  -- Construct the initial state values
  let st = BrickState
            { _stTime         = t
            , _stBTC          = "0" -- ^ Dan Pena
            , _stETH          = "0"
            , _stSOL          = "0"
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
        (K.KChar 'q', [])  -> B.halt  -- Exit on "q" key event
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
      B.padLeft (B.Pad 2) (B.txt (st ^. stBTC))
      <=>
      vtitle "ETHUSDT:"
      <=>
      B.padLeft (B.Pad 2) (B.txt (st ^. stETH))
      <=>
      vtitle "SOLUSDT:"
      <=>
      B.padLeft (B.Pad 2) (B.txt (st ^. stSOL))
      <=>
      B.fill ' '

    timeBlock = time (st ^. stTime)

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
