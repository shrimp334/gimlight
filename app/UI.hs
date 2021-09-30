{-# LANGUAGE OverloadedStrings #-}

module UI(main) where

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (AppEvent, VtyEvent),
                                             EventM, Next, Padding (Pad),
                                             Widget, attrMap, bg, continue,
                                             customMain, fg, hBox, hLimit, halt,
                                             neverShowCursor, on, padAll,
                                             padTop, str, strWrap, vBox, vLimit,
                                             withAttr, withBorderStyle, (<+>),
                                             (<=>))
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center       (center)
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               (use, (&), (^.), (^?!))
import           Control.Monad              (forever, unless, void, when)
import           Control.Monad.Trans.State  (execState, get)
import           Data.Array.Base            ((!))
import           Data.List                  (sortOn)
import           Data.Maybe                 (fromMaybe)
import           Dungeon                    (entities, explored, tileMap,
                                             visible)
import           Dungeon.Map.Tile           (darkAttr, lightAttr)
import           Dungeon.Size               (height, width)
import           Engine                     (Engine (Engine, HandlingEvent, _event),
                                             afterFinish, completeThisTurn,
                                             dungeon, event, initEngine,
                                             isGameOver, messageLog,
                                             playerBumpAction, playerCurrentHp,
                                             playerMaxHp)
import           Entity                     (char, entityAttr, position,
                                             renderOrder)
import           Event                      (numMessages, popMessage)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..), _x, _y)
import qualified Log                        as L

data Tick = Tick

type Name = ()

main :: IO ()
main = do
        chan <- newBChan 10
        forkIO $ forever $ do
            writeBChan chan Tick
            threadDelay 100000
        g <- initEngine
        let builder = V.mkVty V.defaultConfig
        initialVty <- builder
        void $ customMain initialVty builder (Just chan) app g

app :: App Engine Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: Engine -> BrickEvent Name Tick -> EventM Name (Next Engine)
handleEvent e@Engine{} (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KEsc []))        = halt e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KUp []))         = handlePlayerMove (V2 0 1) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KDown []))       = handlePlayerMove (V2 0 (-1)) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KRight []))      = handlePlayerMove (V2 1 0) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KLeft []))       = handlePlayerMove (V2 (-1) 0) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KUpRight [])) = handlePlayerMove (V2 1 1) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KUpLeft [])) = handlePlayerMove (V2 1 (-1)) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KDownRight [])) = handlePlayerMove (V2 (-1) 1) e
handleEvent e@Engine{} (VtyEvent (V.EvKey V.KDownLeft [])) = handlePlayerMove (V2 (-1) (-1)) e
handleEvent e@Engine{} (VtyEvent (V.EvKey (V.KChar 'k') [])) = handlePlayerMove (V2 0 1) e
handleEvent e@Engine{} (VtyEvent (V.EvKey (V.KChar 'j') [])) = handlePlayerMove (V2 0 (-1)) e
handleEvent e@Engine{} (VtyEvent (V.EvKey (V.KChar 'l') [])) = handlePlayerMove (V2 1 0) e
handleEvent e@Engine{} (VtyEvent (V.EvKey (V.KChar 'h') [])) = handlePlayerMove (V2 (-1) 0) e
handleEvent e@HandlingEvent{} (VtyEvent (V.EvKey V.KEnter [])) = handleMessageEvent e
handleEvent e _                                     = continue e

handlePlayerMove :: V2 Int -> Engine -> EventM Name (Next Engine)
handlePlayerMove d e = continue $ flip execState e $ do
    eng <- get
    let finished = eng ^?! isGameOver
    unless finished $ do
        playerBumpAction d
        completeThisTurn

drawUI :: Engine -> [Widget Name]
drawUI e@Engine{} = [ C.center $ drawHpBar e <+> (padTop (Pad 2) (drawGame e) <=> drawMessageLog e)]
drawUI engine@HandlingEvent{} = [withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Roguelike game")
    $ center
    $ padAll 2
    $ strWrap m]
    where
        m = fromMaybe "" (fst $ popMessage (engine ^?! event))

drawGame :: Engine -> Widget Name
drawGame engine@Engine{} = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Game")
    $ vBox rows
    where
        d = engine ^?! dungeon
        rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
        cellsInRow y = [cellAt (V2 x y)  | x <- [0 .. width - 1]]
        coordAsTuple c = (c ^. _x, c ^. _y)
        entityOnCellAt c = [e | e <- d ^. entities, e ^. position == c]
        visibleAt c = (d ^. visible) ! coordAsTuple c
        exploredAt c = (d ^. explored) ! coordAsTuple c
        tileOnCellAt c = (d ^. tileMap) ! coordAsTuple c
        attrAt c
          | visibleAt c = tileOnCellAt c ^. lightAttr
          | exploredAt c = tileOnCellAt c ^. darkAttr
          | otherwise = emptyAttr
        cellAt c = let entityAt = sortOn (^. renderOrder) $ entityOnCellAt c
                       in case entityAt of
                        entity:_ | visibleAt c -> withAttr (entity ^. entityAttr) $ str $ entity ^. char
                        _        -> withAttr (attrAt c) $ str " "
drawGame _ = error "unreachable."

drawMessageLog :: Engine -> Widget Name
drawMessageLog engine = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Log")
    $ vBox rows
    where
        rows = [m | m <- reverse $ fmap (\(attr, s) -> withAttr attr $ str s) $ take L.height $ concatMap (reverse . L.messageToAttrNameAndStringList) (engine ^. messageLog)]

drawStatus :: Engine -> [Widget Name]
drawStatus e = [ C.center $ padTop (Pad 2) (drawHpBar e)]

drawHpBar :: Engine -> Widget Name
drawHpBar e = let barWidth = 20
                  currentHp = playerCurrentHp e
                  maxHp = playerMaxHp e
                  filledWidth = currentHp * barWidth `div` maxHp
                  attrAt x = if x < filledWidth then hpBarFilled else hpBarEmpty
              in vBox [hBox [ x | x <- map (\x -> withAttr (attrAt x) $ str "  ") [0 .. barWidth - 1]], str $ "HP: " ++ show currentHp ++ " / " ++ show maxHp]

handleMessageEvent :: Engine -> EventM Name (Next Engine)
handleMessageEvent e@HandlingEvent{} =
        continue $ if numMessages (e ^?! event) > 1
                    then e { _event = snd $ popMessage (e ^?! event) }
                    else e ^?! afterFinish
handleMessageEvent _ = error "Unreachable."

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, V.black `on` V.rgbColor 200 180 50)
    , (orcAttr, V.rgbColor 63 127 63 `on` V.rgbColor 200 180 50)
    , (trollAttr, V.rgbColor 0 127 0 `on` V.rgbColor 200 180 50)
    , (deadAttr, V.rgbColor 191 0 0 `on` V.rgbColor 200 180 50)
    , (darkFloorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 50 50 150)
    , (lightFloorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 200 180 50)
    , (darkWallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 0 0 100)
    , (lightWallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 130 110 50)
    , (hpBarFilled, bg $ V.rgbColor 0x0 0x60 0x0)
    , (hpBarEmpty, bg $ V.rgbColor 0x40 0x10 0x10)
    , (attackMessageAttr, fg V.red)
    , (infoMessageAttr, fg V.blue)
    ]

playerAttr, npcAttr, emptyAttr, darkFloorAttr, darkWallAttr, orcAttr, trollAttr, infoMessageAttr, hpBarFilled, hpBarEmpty, deadAttr :: AttrName
playerAttr = "playerAttr"
npcAttr = "npcAttr"
emptyAttr = "emptyAttr"
darkFloorAttr = "darkFloorAttr"
lightFloorAttr = "lightFloorAttr"
darkWallAttr = "darkWallAttr"
lightWallAttr = "lightWallAttr"
orcAttr = "orcAttr"
trollAttr = "trollAttr"
attackMessageAttr = "attackMessageAttr"
infoMessageAttr = "infoMessageAttr"
hpBarFilled = "hpBarFilled"
hpBarEmpty = "hpBarEmpty"
deadAttr = "deadAttr"
