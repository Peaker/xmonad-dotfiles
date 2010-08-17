{-# OPTIONS -Wall -O2 -fno-warn-missing-signatures #-}

import XMonad(xmonad, XConfig(..),
              KeyMask, mod4Mask, controlMask,
              (.|.), spawn, sendMessage, kill,
              (-->), composeAll, title, (=?), doShift)
import qualified XMonad as K
import XMonad.Config.Gnome(gnomeConfig)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.WindowNavigation(windowNavigation, Navigate(Go, Swap))
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
import Control.Monad.Trans(MonadIO)
import XMonad.Util.EZConfig
import XMonad.Util.ToggleMaster(newToggleMasterContext, toggleMasterFocus, toggleSwapMasterFocus)
import XMonad.Util.DirKeys(ljikDirs,arrowDirs)
-- import XMonad.Layout
-- import XMonad.Layout.Tabbed
-- import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.EwmhDesktops

meta :: KeyMask
meta = mod4Mask

cmeta :: KeyMask
cmeta = meta .|. controlMask

dzenCmd :: String -> String
dzenCmd msg = "notify-send -t 1500 \"" ++ msg ++ "\""

-- dzen :: (MonadIO m) => String -> m ()
-- dzen = spawn . dzenCmd

execute :: (MonadIO m) => String -> m ()
execute x = spawn $ dzenCmd ("Running " ++ x ++ "!") ++ "& " ++ x

main :: IO ()
main = do
  context <- newToggleMasterContext
  xmonad $ myConfig context

myConfig context = gnomeConfig {
    layoutHook = windowNavigation . smartBorders $ layoutHook gnomeConfig
  , manageHook = composeAll
                 [ manageHook gnomeConfig
                 , title =? "foo" --> doShift "2"
                 , isFullscreen --> doFullFloat
                 ]
  , borderWidth = 3
  , modMask = meta
  , workspaces = map show [(1::Int) .. 6]
  , terminal = "xterm -fg white -bg black"
  } `additionalKeys` myKeys
  where
    myKeys =
        [ ((meta,  K.xK_f),      execute "firefox")
        , ((meta,  K.xK_g),      execute "google-chrome")
        , ((meta,  K.xK_a),      execute "Thunar")
        -- , ((meta,  K.xK_g),      execute "glxgears")
        , ((meta,  K.xK_c),      kill)
        , ((meta,  K.xK_m),      toggleMasterFocus context)
        , ((meta,  K.xK_Return), toggleSwapMasterFocus context)
        ] ++ [((m, key), sendMessage $ c dir)
             | (m, c) <- [(meta, Go), (cmeta, Swap)]
             , keyDirs <- [arrowDirs]
             , (key, dir) <- keyDirs]
