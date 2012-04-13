import XMonad
import XMonad.Config.Gnome
import Utils 
import Dzen                       (DzenConf(..), spawnDzen, spawnToDzen, defaultDzen,myRightBar,myLeftBar)
import ScratchPadKeys             (scratchPadList, manageScratchPads, scratchPadKeys)
import System.IO                  (hPutStrLn)
import XMonad.Hooks.DynamicLog    (dynamicLogWithPP, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook   (withUrgencyHookC)
import XMonad.Layout.Maximize
import XMonad.Util.EZConfig       (additionalKeys)
import XMonad.Hooks.SetWMName


myLayout = avoidStruts $ maximize (Tall 1 (3/100) (1/2)) ||| Full |||  pbLayout

myManageHook :: ManageHook
myManageHook = composeAll 
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    ]

main :: IO ()
main = do
      xmonad gnomeConfig { manageHook = myManageHook } 
      xmonad $ withUrgencyHookC pbUrgencyHook pbUrgencyConfig $ defaultConfig
        { terminal    = "gnome-terminal"
        , workspaces  = pbWorkspaces
        , layoutHook  = myLayout
        , manageHook  = pbManageHook <+> myManageHook
        -- , logHook     = dynamicLogWithPP $ pbPP { ppOutput = hPutStrLn d }
     --   , startupHook = setWMName "LG3D"
     --   , startupHook = startupHook desktopConfig >> setWMName "LG3D"
      , startupHook = setWMName "LG3D"
       -- , modMask = mod4Mask     -- Rebind Mod to the Windows key
        }

