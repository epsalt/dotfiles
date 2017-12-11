import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import System.IO

myWorkspaces = [ "one", "two", "three", "four", "five" ]

myLayout = avoidStruts $
           tiled |||
           Mirror tiled |||
           threeCol |||
           full |||
           maxFull

           where
             tiled = renamed [Replace "tiled"] $ spacing 15 $ Tall 1 (3/100) (1/2)
             threeCol = renamed [Replace "3col"] $ spacing 15 $ ThreeCol 1 (3/100) (1/3)
             full = renamed [Replace "full"] $ spacing 15 $ Full
             maxFull = renamed [Replace "max"] $ noBorders $ Full

myKeys = [ ((mod4Mask, xK_q), spawn "chromium-browser")
         , ((mod4Mask, xK_a), spawn "emacs-snapshot")
         ]

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor = "#000000"
myFocusedBorderColor = "#000000"

myTerminal = "urxvt"

main = do
    spawn "feh --bg-scale ~/.xmonad/jp.jpg"
    xmproc <- spawnPipe "/usr/bin/xmobar /home/epsalt/.xmobarrc"
    xmonad $ defaultConfig {
      modMask = mod4Mask,
      manageHook = manageDocks <+> manageHook defaultConfig,
      handleEventHook = docksEventHook <+> handleEventHook defaultConfig,
      layoutHook = myLayout,
      logHook = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn xmproc },
      workspaces = myWorkspaces,
      terminal = myTerminal,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor
      }`additionalKeys` myKeys
