{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.Exploring
    ( drawExploring
    ) where

import           Codec.Picture                   (Image (imageData, imageHeight, imageWidth))
import           Control.Lens                    (Ixed (ix), (&), (.~), (^.),
                                                  (^?))
import           Control.Monad                   (guard)
import           Data.Array                      ((!))
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, mapMaybe)
import           Data.Vector.Storable.ByteString (vectorToByteString)
import           Gimlight.Actor                  (getArmor,
                                                  getCurrentExperiencePoint,
                                                  getDefence,
                                                  getExperiencePointForNextLevel,
                                                  getHp, getLevel, getMaxHp,
                                                  getPower, getWeapon,
                                                  walkingImagePath)
import           Gimlight.Coord                  (Coord)
import           Gimlight.Dungeon                (Dungeon, cellMap)
import           Gimlight.Dungeon.Map.Cell       (exploredMap, lower,
                                                  playerActor, playerFov,
                                                  positionsAndActors,
                                                  positionsAndItems,
                                                  tileIdLayerAt, upper,
                                                  widthAndHeight)
import           Gimlight.Dungeon.Map.Tile       (getImage)
import           Gimlight.GameConfig             (GameConfig)
import           Gimlight.GameStatus.Exploring   (ExploringHandler,
                                                  getCurrentDungeon,
                                                  getMessageLog, getPlayerActor,
                                                  getTileCollection)
import           Gimlight.Item                   (getName)
import           Gimlight.Item.SomeItem          (getIconImagePath)
import           Gimlight.Localization           (getLocalizedText)
import qualified Gimlight.Localization.Texts     as T
import           Gimlight.UI.Draw.Config         (logRows, tileColumns,
                                                  tileHeight, tileRows,
                                                  tileWidth, windowWidth)
import           Gimlight.UI.Draw.KeyEvent       (withKeyEvents)
import           Gimlight.UI.Types               (GameWidgetNode)
import           Linear.V2                       (V2 (V2), _x, _y)
import           Monomer                         (CmbBgColor (bgColor),
                                                  CmbHeight (height),
                                                  CmbMultiline (multiline),
                                                  CmbPaddingL (paddingL),
                                                  CmbPaddingT (paddingT),
                                                  CmbStyleBasic (styleBasic),
                                                  CmbWidth (width), Size (Size),
                                                  black, filler, hstack, image,
                                                  imageMem, label, label_,
                                                  vstack, zstack)
import qualified Monomer.Lens                    as L
import           TextShow                        (TextShow (showt))

drawExploring :: ExploringHandler -> GameConfig -> GameWidgetNode
drawExploring eh c =
    withKeyEvents $ vstack [statusAndMapGrid, messageLogArea eh c]
  where
    statusAndMapGrid =
        hstack
            [ mapGrid eh
            , statusGrid eh c `styleBasic`
              [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
            ]

messageLogArea :: ExploringHandler -> GameConfig -> GameWidgetNode
messageLogArea eh c =
    vstack $
    fmap (\x -> label_ (getLocalizedText c x) [multiline]) $
    take logRows $ getMessageLog eh

mapGrid :: ExploringHandler -> GameWidgetNode
mapGrid eh =
    zstack (mapWidget eh : (mapItems eh ++ mapActors eh)) `styleBasic`
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
             , label $ wp <> ": " <> wpName x
             , label $ am <> ": " <> amName x
             ]) $
    getPlayerActor eh
  where
    lvl = getLocalizedText c T.level
    experience = getLocalizedText c T.experience
    atk = getLocalizedText c T.attack
    defence = getLocalizedText c T.defence
    wp = getLocalizedText c T.weapon
    am = getLocalizedText c T.armor
    wpName = itemNameOrEmpty . getWeapon
    amName = itemNameOrEmpty . getArmor
    itemNameOrEmpty = maybe mempty (getLocalizedText c . getName)

mapWidget :: ExploringHandler -> GameWidgetNode
mapWidget eh = vstack rows
  where
    rows = [row y | y <- [topLeftCoordY .. topLeftCoordY + tileRows - 1]]
    row y = hstack $ columns y
    columns y =
        [ cell (V2 x y)
        | x <- [topLeftCoordX .. topLeftCoordX + tileColumns - 1]
        ]
    cell c =
        zstack (catMaybes [lowerLayerAt c, upperLayerAt c, Just $ shadowAt c]) `styleBasic`
        [width $ fromIntegral tileWidth, height $ fromIntegral tileHeight]
    lowerLayerAt = layerOfAt lower
    upperLayerAt = layerOfAt upper
    layerOfAt which c = tileIdToImageMem <$> getTileIdOfLayerAt which c
    tileIdToImageMem tileId =
        imageMem
            (showt tileId)
            (vectorToByteString $ imageData img)
            (imgSize img)
      where
        img = getImage $ getTileCollection eh Map.! tileId
    imgSize img =
        Size (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img)
    shadowAt c = filler `styleBasic` [bgColor $ black & L.a .~ cellOpacity c]
    cellOpacity c
        | isVisible c = 0
        | isExplored c = 0.5
        | otherwise = 1
    isVisible c = playerFov (d ^. cellMap) ^? ix c == Just True
    isExplored c = exploredMap (d ^. cellMap) ^? ix c == Just True
    getTileIdOfLayerAt which c = tileIdLayer c >>= (^. which)
    tileIdLayer c = tileIdLayerAt c $ d ^. cellMap
    V2 topLeftCoordX topLeftCoordY = topLeftCoord d
    d = getCurrentDungeon eh

mapItems :: ExploringHandler -> [GameWidgetNode]
mapItems eh = mapMaybe itemToImage $ positionsAndItems $ d ^. cellMap
  where
    itemToImage (position, item) =
        guard (isItemDrawed position) >>
        return (image (getIconImagePath item) `styleBasic` style position)
    isItemDrawed position =
        let displayPosition = itemPositionOnDisplay position
            isVisible = playerFov (d ^. cellMap) ! position
         in V2 0 0 <= displayPosition &&
            displayPosition < V2 tileColumns tileRows && isVisible
    d = getCurrentDungeon eh
    leftPadding position =
        fromIntegral $ itemPositionOnDisplay position ^. _x * tileWidth
    topPadding position =
        fromIntegral $ itemPositionOnDisplay position ^. _y * tileHeight
    style position =
        [paddingL $ leftPadding position, paddingT $ topPadding position]
    itemPositionOnDisplay position = position - topLeftCoord d

mapActors :: ExploringHandler -> [GameWidgetNode]
mapActors eh = mapMaybe actorToImage . positionsAndActors $ d ^. cellMap
  where
    d = getCurrentDungeon eh
    leftPadding actor =
        fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
    topPadding actor =
        fromIntegral $ actorPositionOnDisplay actor ^. _y * tileHeight
    style position =
        [paddingL $ leftPadding position, paddingT $ topPadding position]
    actorPositionOnDisplay position = position - topLeftCoord d
    isActorDrawed position =
        let displayPosition = actorPositionOnDisplay position
            isVisible = playerFov (d ^. cellMap) ! position
         in V2 0 0 <= displayPosition &&
            displayPosition < V2 tileColumns tileRows && isVisible
    actorToImage (position, actor) =
        guard (isActorDrawed position) >>
        return (image (actor ^. walkingImagePath) `styleBasic` style position)

topLeftCoord :: Dungeon -> Coord
topLeftCoord d = V2 x y
  where
    V2 unadjustedX unadjestedY =
        maybe
            (V2 0 0)
            ((\pos -> pos - V2 (tileColumns `div` 2) (tileRows `div` 2)) . fst)
            (playerActor $ d ^. cellMap)
    V2 maxX maxY = widthAndHeight (d ^. cellMap) - V2 tileColumns tileRows
    x = max 0 $ min maxX unadjustedX
    y = max 0 $ min maxY unadjestedY

mapDrawingWidth :: Int
mapDrawingWidth = tileWidth * tileColumns

mapDrawingHeight :: Int
mapDrawingHeight = tileHeight * tileRows
