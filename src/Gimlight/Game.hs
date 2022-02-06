{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Game
    ( start
    ) where

import           Gimlight.GameConfig     (getLocale, readConfigOrDefault)
import           Gimlight.GameModel      (GameModel (GameModel, config, status))
import           Gimlight.GameStatus     (GameStatus (SelectingLocale, Title))
import           Gimlight.UI.Draw        (drawUI)
import           Gimlight.UI.Draw.Config (windowHeight, windowWidth)
import           Gimlight.UI.Draw.Fonts  (bold, regular)
import qualified Gimlight.UI.Event       as E
import           Gimlight.UI.Types       (AppEvent (..))
import           Monomer                 (Font (unFont),
                                          MainWindowState (MainWindowNormal),
                                          appFontDef, appInitEvent, appTheme,
                                          appWindowResizable, appWindowState,
                                          appWindowTitle, darkTheme, startApp)

start :: IO ()
start = do
    initModel <- createModel
    startApp initModel handleEvent buildUI initUIConfig
  where
    createModel = do
        initConfig <- readConfigOrDefault
        let initStatus =
                case getLocale initConfig of
                    Just _  -> Title
                    Nothing -> SelectingLocale
        return GameModel {status = initStatus, config = initConfig}
    handleEvent = E.handleEvent
    buildUI = drawUI
    initUIConfig =
        [ appWindowTitle "Roguelike"
        , appTheme darkTheme
        , appFontDef
              (unFont regular)
              "third_party/Noto_Sans_JP/NotoSansJP-Light.otf"
        , appFontDef
              (unFont bold)
              "third_party/Noto_Sans_JP/NotoSansJP-Bold.otf"
        , appInitEvent AppInit
        , appWindowState $ MainWindowNormal (windowWidth, windowHeight)
        , appWindowResizable False
        ]
