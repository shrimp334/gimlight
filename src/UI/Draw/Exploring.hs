{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Exploring
    ( drawExploring
    ) where

import           Actor                           (getCurrentExperiencePoint,
                                                  getDefence,
                                                  getExperiencePointForNextLevel,
                                                  getHp, getLevel, getMaxHp,
                                                  getPower, walkingImagePath)
import qualified Actor                           as A
import           Codec.Picture                   (Image (imageData),
                                                  PixelRGBA8 (PixelRGBA8),
                                                  pixelMap)
import           Control.Applicative             (ZipList (ZipList, getZipList))
import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import           Coord                           (Coord)
import           Data.Array                      ((!))
import           Data.Default                    (Default (def))
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           Data.Vector.Split               (chunksOf)
import qualified Data.Vector.Storable            as V
import           Data.Vector.Storable.ByteString (vectorToByteString)
import           Dungeon                         (Dungeon, explored, getActors,
                                                  items, mapWidthAndHeight,
                                                  playerPosition, tileMap,
                                                  visible)
import           Dungeon.Map.Tile                (tileIdAt)
import           GameConfig                      (GameConfig)
import           GameStatus.Exploring            (ExploringHandler,
                                                  getCurrentDungeon,
                                                  getMessageLog, getPlayerActor)
import qualified Item                            as I
import           Linear.V2                       (V2 (V2), _x, _y)
import           Localization                    (getLocalizedText)
import qualified Localization.Texts              as T
import           Monomer                         (CmbHeight (height),
                                                  CmbMultiline (multiline),
                                                  CmbPaddingL (paddingL),
                                                  CmbPaddingT (paddingT),
                                                  CmbStyleBasic (styleBasic),
                                                  CmbWidth (width),
                                                  Point (Point), Rect (Rect),
                                                  Renderer (addImage, beginPath, deleteImage, fill, setFillImagePattern),
                                                  Size (Size), Widget,
                                                  WidgetNode, currentStyle,
                                                  defaultWidgetNode,
                                                  drawRoundedRect,
                                                  getContentArea, hstack, image,
                                                  label, label_, vstack, zstack)
import           Monomer.Widgets.Single          (Single (singleRender),
                                                  createSingle)
import           TextShow                        (TextShow (showt))
import           UI.Draw.Config                  (logRows, tileColumns,
                                                  tileHeight, tileRows,
                                                  tileWidth, windowWidth)
import           UI.Draw.KeyEvent                (withKeyEvents)
import           UI.Graphics.MapTiles            (MapTiles)
import           UI.Types                        (GameWidgetNode)

drawExploring :: MapTiles -> ExploringHandler -> GameConfig -> GameWidgetNode
drawExploring tileGraphics eh c =
    withKeyEvents $ vstack [statusAndMapGrid, messageLogArea eh c]
  where
    statusAndMapGrid =
        hstack
            [ mapGrid tileGraphics eh
            , statusGrid eh c `styleBasic`
              [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
            ]

messageLogArea :: ExploringHandler -> GameConfig -> GameWidgetNode
messageLogArea eh c =
    vstack $
    fmap (\x -> label_ (getLocalizedText c x) [multiline]) $
    take logRows $ getMessageLog eh

mapGrid :: MapTiles -> ExploringHandler -> GameWidgetNode
mapGrid tileGraphics eh =
    zstack (mapWidget tileGraphics eh : (mapItems eh ++ mapActors eh)) `styleBasic`
    [ width $ fromIntegral mapDrawingWidth
    , height $ fromIntegral mapDrawingHeight
    ]

statusGrid :: ExploringHandler -> GameConfig -> GameWidgetNode
statusGrid eh c =
    vstack $
    maybe
        []
        (\x ->
             [ label "Player"
             , label $ lvl <> ": " <> showt (getLevel x)
             , label $
               experience <>
               ": " <>
               showt (getCurrentExperiencePoint x) <>
               " / " <> showt (getExperiencePointForNextLevel x)
             , label $ "HP: " <> showt (getHp x) <> " / " <> showt (getMaxHp x)
             , label $ atk <> ": " <> showt (getPower x)
             , label $ defence <> ": " <> showt (getDefence x)
             ]) $
    getPlayerActor eh
  where
    lvl = getLocalizedText c T.level
    experience = getLocalizedText c T.experience
    atk = getLocalizedText c T.attack
    defence = getLocalizedText c T.defence

mapWidget :: MapTiles -> ExploringHandler -> WidgetNode s e
mapWidget tiles eh = defaultWidgetNode "map" $ makeMap tiles eh

makeMap :: MapTiles -> ExploringHandler -> Widget s e
makeMap tileGraphics eh = createSingle () def {singleRender = render}
  where
    render wenv node renderer = do
        addImage renderer imagePath mapSize rows []
        beginPath renderer
        setFillImagePattern
            renderer
            imagePath
            (Point x y)
            (Size w h)
            angle
            transparent
        drawRoundedRect renderer (Rect x y w h) def
        fill renderer
        deleteImage renderer imagePath
      where
        style = currentStyle wenv node
        Rect x y w h = getContentArea node style
        angle = 0
        transparent = 1
    rows =
        vectorToByteString $
        V.concat [row y | y <- [topLeftCoordY .. topLeftCoordY + tileRows - 1]]
    row y =
        V.concat $
        getZipList $
        foldl1
            (\acc x -> (V.++) <$> acc <*> x)
            [ ZipList $ imageAt $ V2 x y
            | x <- [topLeftCoordX .. topLeftCoordX + tileColumns - 1]
            ]
    imageAt c =
        chunksOf (tileWidth * 4) $ -- `(*4)` for R, G, B, and A bytes.
        imageData $ pixelMap (applyOpacity c) $ tileGraphics ! tileId c
    tileId c =
        fromMaybe
            (error "Failed to get the tile ID.")
            (tileIdAt c (d ^. tileMap))
    applyOpacity c (PixelRGBA8 r g b a)
        | isVisible c = PixelRGBA8 r g b a
        | isExplored c = PixelRGBA8 (r `div` 2) (g `div` 2) (b `div` 2) a
        | otherwise = PixelRGBA8 0 0 0 0xff
    isVisible c = (d ^. visible) ! c
    isExplored c = (d ^. explored) ! c
    d = getCurrentDungeon eh
    V2 topLeftCoordX topLeftCoordY = topLeftCoord d
    imagePath = "mapWidget"

mapItems :: ExploringHandler -> [GameWidgetNode]
mapItems eh = mapMaybe itemToImage $ d ^. items
  where
    itemToImage item =
        guard (isItemDrawed item) >>
        return (image (I.getIconImagePath item) `styleBasic` style item)
    isItemDrawed item =
        let displayPosition = itemPositionOnDisplay item
            isVisible = (d ^. visible) ! I.getPosition item
         in V2 0 0 <= displayPosition &&
            displayPosition < V2 tileColumns tileRows && isVisible
    d = getCurrentDungeon eh
    leftPadding item =
        fromIntegral $ itemPositionOnDisplay item ^. _x * tileWidth
    topPadding item =
        fromIntegral $ itemPositionOnDisplay item ^. _y * tileHeight
    style item = [paddingL $ leftPadding item, paddingT $ topPadding item]
    itemPositionOnDisplay item = I.getPosition item - topLeftCoord d

mapActors :: ExploringHandler -> [GameWidgetNode]
mapActors eh = mapMaybe actorToImage $ getActors d
  where
    d = getCurrentDungeon eh
    leftPadding actor =
        fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
    topPadding actor =
        fromIntegral $ (actorPositionOnDisplay actor ^. _y) * tileHeight
    style actor = [paddingL $ leftPadding actor, paddingT $ topPadding actor]
    actorPositionOnDisplay actor = actor ^. A.position - topLeftCoord d
    isActorDrawed actor =
        let displayPosition = actorPositionOnDisplay actor
            isVisible = (d ^. visible) ! (actor ^. A.position)
         in V2 0 0 <= displayPosition &&
            displayPosition < V2 tileColumns tileRows && isVisible
    actorToImage actor =
        guard (isActorDrawed actor) >>
        return (image (actor ^. walkingImagePath) `styleBasic` style actor)

topLeftCoord :: Dungeon -> Coord
topLeftCoord d = V2 x y
  where
    V2 unadjustedX unadjestedY =
        maybe
            (V2 0 0)
            (\pos -> pos - V2 (tileColumns `div` 2) (tileRows `div` 2))
            (playerPosition d)
    V2 maxX maxY = mapWidthAndHeight d - V2 tileColumns tileRows
    x = max 0 $ min maxX unadjustedX
    y = max 0 $ min maxY unadjestedY

mapSize :: Size
mapSize = Size (fromIntegral mapDrawingWidth) (fromIntegral mapDrawingHeight)

mapDrawingWidth :: Int
mapDrawingWidth = tileWidth * tileColumns

mapDrawingHeight :: Int
mapDrawingHeight = tileHeight * tileRows
