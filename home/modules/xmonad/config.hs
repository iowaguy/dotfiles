import XMonad
-- import XMonad.ManageHook
-- import XMonad.Config.Desktop
-- import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
-- import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce (spawnOnce, spawnOnOnce)
import XMonad.Actions.SpawnOn (spawnOn, manageSpawn)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
-- import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
-- import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, hPutStrLn)
-- import Data.List (sortBy)
-- import Data.Function (on)
-- import Control.Monad (forM_, join)
-- import XMonad.Util.Run (safeSpawn)
-- import XMonad.Util.NamedWindows (getName)

-- Colours
gray      = "#7F7F7F"
gray2     = "#222222"
red       = "#900000"
blue      = "#2E9AFE"
white     = "#eeeeee"
orange    = "#ff9604"

myTerminal :: String
myTerminal = "kitty"

-- | Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 3

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:emacs", "2:shell", "3:web", "4:zotero", "5:chat", "6:zoom", "7:music", "8:notion", "9:pdfs"] -- ++ map show [8..9]

-- | Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = "gray" -- "#dddddd"
myFocusedBorderColor = "blue"  -- "#ff0000" don't use hex, not <24 bit safe

-- | Perform an arbitrary action at xmonad startup.
myStartupHook :: X ()
myStartupHook = composeAll
                  [
                    -- spawnOnce "1" "em"
                    spawnOnOnce "2:shell" myTerminal
                  , spawnOnOnce "3:web" "brave"
                  , spawnOnOnce "4:zotero" "zotero"
                  , spawnOnOnce "5:chat" "signal-desktop"
                  , spawnOnOnce "5:chat" "slack"
                  , spawnSingleProcess "stalonetray"
                  ]

spawnSingleProcess p =
  spawnOnce $ "if [ -z $(pgrep " <> p <> ") ] ; then " <> p <> " & fi"

myManageHook :: ManageHook
myManageHook =
  composeAll
      [ className =? "standalonetray" --> doIgnore
      , className =? "brave"          --> doShift "3:web"
      ]

launcherString :: String
launcherString = "rofi -show run -modi \"filebrowser#run#ssh#calc\" -no-show-match -no-sort -calc-command \"echo -n '{result}' | xclip -selection clipboard\""

xmobarTop :: StatusBarConfig
xmobarTop = statusBarPropTo "_XMONAD_LOG_1" "xmobar ~/.config/xmobar/xmobarrc_top" $ pure xmobarPP

xmobarBottom :: StatusBarConfig
xmobarBottom = statusBarPropTo "_XMONAD_LOG_2" "xmobar ~/.config/xmobar/xmobarrc_bottom" $ pure xmobarPP {
    ppCurrent = xmobarColor "black" "orange"
  , ppTitle   = xmobarColor "white" "" . shorten 40
  , ppUrgent  = xmobarColor "white" "red"
  }

main :: IO ()
main = xmonad $ ewmh $ ewmhFullscreen $ withEasySB (xmobarTop <> xmobarBottom) defToggleStrutsKey def
        { modMask = mod1Mask -- Use Alt
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook = myManageHook <+> manageSpawn
        , startupHook = myStartupHook
        } `additionalKeysP`
          [("<Print>", spawn "flameshot gui")
          , ("M-d", spawn launcherString)
          , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status")
          , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status")
          , ("<XF86MonBrightnessUp>", spawn "brightnessctl -d 'intel_backlight' set +10%")
          , ("<XF86MonBrightnessDown>", spawn "brightnessctl -d 'intel_backlight' set 10%-")
          , ("<XF86AudioMute>",      spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status")
          , ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status")
          , ("S-M-p", spawn "rofi-pass")
          , ("S-M-q", kill)
          , ("M-<return>", spawn myTerminal)
          ]
