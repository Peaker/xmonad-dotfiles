{-# OPTIONS -Wall -O2 -fno-warn-missing-signatures #-}

import XMonad(xmonad, XConfig(..),
              KeyMask, mod4Mask, controlMask,
              (.|.), spawn, sendMessage, kill)
import qualified XMonad as K
import XMonad.Config.Xfce(xfceConfig)
-- import XMonad.Layout
-- import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.WindowNavigation(windowNavigation, Navigate(Go, Swap))
-- import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
-- import XMonad.Hooks.EwmhDesktops
import Control.Monad.Trans(MonadIO)

import XMonad.Util.ToggleMaster(newToggleMasterContext, toggleMasterFocus, toggleSwapMasterFocus)
import XMonad.Util.DirKeys(ljikDirs,arrowDirs)

meta :: KeyMask
meta = mod4Mask

cmeta :: KeyMask
cmeta = meta .|. controlMask

dzenCmd :: String -> String
dzenCmd msg = "echo " ++ msg ++ " | dzen2 -p 2 -w 180 -h 50 "

-- dzen :: (MonadIO m) => String -> m ()
-- dzen = spawn . dzenCmd

execute :: (MonadIO m) => String -> m ()
execute x = spawn $ dzenCmd ("Running " ++ x ++ "!") ++ "& " ++ x

main :: IO ()
main = do
  context <- newToggleMasterContext
  xmonad $ myConfig context

myConfig context = xfceConfig {
    layoutHook = windowNavigation . smartBorders $ layoutHook xfceConfig
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
