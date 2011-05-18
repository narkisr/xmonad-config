-------------------------------------------------------------------------------
-- Ronen see http://tinyurl.com/3jeej67
-- ;
-- Module : xmonad.hs
-- Copyright : (c) Patrick Brisbin 2010
-- License : as-is
--
-- Maintainer : pbrisbin@gmail.com
-- Stability : unstable
-- Portability : unportable
--
-- http://github.com/pbrisbin/xmonad-config/
--
-------------------------------------------------------------------------------
--
import XMonad

-- <http://pbrisbin.com/xmonad/docs/Utils.html>
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
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName


myLayout = avoidStruts $ maximize (Tall 1 (3/100) (1/2)) ||| Full |||  pbLayout

main :: IO ()
main = do
    d <- spawnDzen myLeftBar
    spawnToDzen "conky -c ~/.dzen_conkyrc" myRightBar
    spawn "~/.dropbox-dist/dropboxd"
    xmonad $ withUrgencyHookC pbUrgencyHook pbUrgencyConfig $ defaultConfig
        { terminal    = "gnome-terminal"
        , workspaces  = pbWorkspaces
        , layoutHook  = myLayout
        , manageHook  = pbManageHook <+> myManageHook
        , logHook     = dynamicLogWithPP $ pbPP { ppOutput = hPutStrLn d }
        , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
       , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys` 
       [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock") --mod4mask is the windows key
      , ((0, xK_Print), spawn "gnome-screenshot")
      , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
      , ((mod4Mask, xK_m), withFocused (sendMessage . maximizeRestore))
      , ((mod4Mask .|. controlMask , xK_h ), spawn "setxkbmap -layout il")
      , ((mod4Mask , xK_e ), spawn "setxkbmap -layout us")
      , ((mod4Mask , xK_q ), cleanStart)
      ]

myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ] <+> manageScratchPads scratchPadList
    where myActions = [ ("rdesktop"  , doFloat         )
                      , ("Xmessage"  , doCenterFloat   )
                      , ("Gmrun"     , doCenterFloat   )
                      , ("Firefox"      , doShift "2-web" )
                      , ("Uzbl"      , doShift "2-web" )
                      , ("Uzbl-core" , doShift "2-web" )
                      , ("Chromium"  , doShift "2-web" )
                      ]
