import XMonad
-- import XMonad.ManageHook
-- import XMonad.Config.Desktop
-- import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
-- import XMonad.Util.Ungrab
-- import XMonad.Util.SpawnOnce (spawnOnce)
-- import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
-- import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
-- import qualified DBus as D
-- import qualified DBus.Client as D
-- import qualified Codec.Binary.UTF8.String as UTF8
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
myWorkspaces = ["1:emacs", "2:shell", "3:web", "4:zotero", "5:chat", "6:zoom", "7:music"] ++ map show [8..9]

-- | Border colors for unfocused and focused windows, respectively.
--
-- myNormalBorderColor, myFocusedBorderColor :: String
-- myNormalBorderColor  = "gray" -- "#dddddd"
-- myFocusedBorderColor = "blue"  -- "#ff0000" don't use hex, not <24 bit safe

-- | Perform an arbitrary action at xmonad startup.
-- myStartupHook :: X ()
-- myStartupHook = composeAll
--                   [
--                     -- spawnOnce "1" "em"
--                     dynStatusBarStartup barCreator1 barDestroyer
--                   -- , dynStatusBarStartup barCreator2 barDestroyer
--                   , spawnOnOnce "2:shell" myTerminal
--                   , spawnOnOnce "3:web" "firefox"
--                   , spawnOnOnce "4:zotero" "zotero"
--                   , spawnOnOnce "5:chat" "signal-desktop"
--                   -- , spawnSingleProcess "status-notifier-watcher"
--                   ]

-- spawnSingleProcess p =
--   spawnOnce $ "if [ -z $(pgrep " <> p <> ") ] ; then " <> p <> " & fi"


launcherString :: String
launcherString = "rofi -show run -modi \"filebrowser#run#ssh#calc\" -no-show-match -no-sort -calc-command \"echo -n '{result}' | xclip -selection clipboard\""

mySB :: StatusBarConfig
mySB = statusBarProp "xmobar" $ pure xmobarPP {
    ppCurrent = xmobarColor "black" "orange"
  , ppTitle   = xmobarColor "white" "" . shorten 40
  , ppUrgent  = xmobarColor "white" "red"
  }

main :: IO ()
main = xmonad $ ewmh $ ewmhFullscreen $ withEasySB mySB defToggleStrutsKey def
        { modMask = mod1Mask -- Use Alt
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        } `additionalKeysP`
          [("<Print>", spawn "flameshot gui")
          , ("M-d", spawn launcherString)
          , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status")
          , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status")
          , ("<XF86MonBrightnessUp>", spawn "brightnessctl -d 'intel_backlight' +50")
          , ("<XF86MonBrightnessDown>", spawn "brightnessctl -d 'intel_backlight' +50")
          , ("<XF86AudioMute>",      spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status")
          , ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status")
          , ("S-M-p", spawn "rofi-pass")
          , ("S-M-q", kill)
          , ("M-<return>", spawn myTerminal)
          ]
