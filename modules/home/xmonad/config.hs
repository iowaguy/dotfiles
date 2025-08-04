import XMonad
import XMonad.Actions.CycleWS
  ( nextScreen,
    prevScreen,
    shiftNextScreen,
    shiftPrevScreen,
    swapNextScreen,
  )
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar
  ( StatusBarConfig,
    defToggleStrutsKey,
    statusBarPropTo,
    withEasySB,
  )
import XMonad.Hooks.StatusBar.PP
  ( PP (ppCurrent, ppHidden, ppHiddenNoWindows, ppSort, ppTitle, ppUrgent),
    shorten,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Layout.BinarySpacePartition as BSP
  ( BinarySpacePartition,
    FocusParent (FocusParent),
    Rotate (Rotate),
    SelectMoveNode (MoveNode, SelectNode),
    Swap (Swap),
    emptyBSP,
  )
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.Spacing (Spacing, spacing)
import XMonad.StackSet (tag)
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    defaultFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnOnce, spawnOnce)
import XMonad.Util.WorkspaceCompare (filterOutWs, getSortByIndex)

-- Colours
gray = "#7F7F7F"

gray2 = "#222222"

red = "#900000"

blue = "#2E9AFE"

white = "#eeeeee"

orange = "#ff9604"

myTerminal :: String
myTerminal = "kitty"

myNotes :: String
myNotes = "obsidian"

-- | Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 3

myLayout :: ModifiedLayout Spacing (Choose (ModifiedLayout SmartBorder Grid) (Choose (ModifiedLayout SmartBorder Full) (ModifiedLayout SmartBorder BinarySpacePartition))) Window
myLayout = spacing 10 $ layoutGrid ||| layoutFull ||| layoutBinarySpacePartition
  where
    layoutGrid = smartBorders Grid
    layoutFull = smartBorders Full
    layoutBinarySpacePartition = smartBorders emptyBSP

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:code", "2:shell", "3:web", "4:comms", "5:sharing", "6", "7", "8", "9"]

-- | Border colors for unfocused and focused windows, respectively.
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "gray" -- "#dddddd"
myFocusedBorderColor = "blue" -- "#ff0000" dont use hex, not <24 bit safe

-- | Perform an arbitrary action at xmonad startup.
myStartupHook :: X ()
myStartupHook =
  composeAll
    [ spawnOnOnce "3:web" "brave",
      spawnOnOnce "2:shell" myTerminal,
      spawnRestart "stalonetray",
      spawnRestart "nm-applet"
    ]

spawnRestart :: (MonadIO m) => String -> m ()
spawnRestart p =
  spawn $ "pkill -9 " <> p <> "; " <> p

spawnSingleProcess :: (MonadIO m) => String -> m ()
spawnSingleProcess p =
  spawn $ "if test -z $(pgrep " <> p <> "); then " <> p <> "; fi"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "brave" --> doShift "3:web",
      namedScratchpadManageHook myScratchPads,
      title =? "Dunst" --> defaultFloating
    ]

launcherString :: String
launcherString = "rofi -show drun -no-show-match -no-sort -calc-command \"echo -n '{result}' | xclip -selection clipboard\""

xmobarTop :: StatusBarConfig
xmobarTop = statusBarPropTo "_XMONAD_LOG_1" "xmobar -v ~/.config/xmobar/xmobarrc_top" $ pure xmobarPP

xmobarBottom :: StatusBarConfig
xmobarBottom =
  statusBarPropTo "_XMONAD_LOG_2" "xmobar -v ~/.config/xmobar/xmobarrc_bottom" $
    ( clickablePP
        xmobarPP
          { ppCurrent = xmobarColor "black" "orange",
            ppTitle = xmobarColor "white" "" . shorten 40,
            ppUrgent = xmobarColor "white" "red",
            ppHidden = \ws -> if ws == "NSP" then "" else xmobarColor "white" "" ws,
            ppHiddenNoWindows = \ws -> if ws == "NSP" then "" else xmobarColor "grey" "" ws,
            ppSort = do
              return $ filterOutWs [scratchpadWorkspaceTag] -- <$> getSortByIndex
          }
    )

-- Use Alt
myModMask :: KeyMask
myModMask = mod1Mask

_XF86AudioMute, _XF86AudioRaiseVolume, _XF86AudioLowerVolume, _XF86MonBrightnessUp, _XF86MonBrightnessDown, _XF86AudioMicMute :: KeySym
_XF86AudioMute = 0x1008ff12
_XF86AudioRaiseVolume = 0x1008ff13
_XF86AudioLowerVolume = 0x1008ff11
_XF86MonBrightnessUp = 0x1008ff02
_XF86MonBrightnessDown = 0x1008ff03
_XF86AudioMicMute = 0x1008ffb2

myKeys :: KeyMask -> [((ButtonMask, KeySym), X ())]
myKeys modMask =
  [ ((noModMask, xK_Print), spawn "flameshot gui"),
    ((modMask, xK_d), spawn launcherString),
    ((modMask .|. shiftMask, xK_p), spawn "rofi-pass"),
    ((modMask .|. shiftMask, xK_q), kill),
    ((noModMask, _XF86AudioRaiseVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%+ && $refresh_i3status"),
    ((noModMask, _XF86AudioLowerVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%- && $refresh_i3status"),
    ((noModMask, _XF86MonBrightnessUp), spawn "brightnessctl -d 'intel_backlight' set +10%"),
    ((noModMask, _XF86MonBrightnessDown), spawn "brightnessctl -d 'intel_backlight' set 10%-"),
    ((noModMask, _XF86AudioMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle && $refresh_i3status"),
    ((noModMask, _XF86AudioMicMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle && $refresh_i3status"),
    -- -- Used by BinarySpacePartition layout
    ((modMask, xK_r), sendMessage Rotate),
    ((modMask, xK_s), sendMessage Swap),
    ((modMask, xK_n), sendMessage FocusParent),
    ((modMask .|. controlMask, xK_n), sendMessage SelectNode),
    ((modMask .|. shiftMask, xK_n), sendMessage MoveNode),
    ((modMask, xK_o), namedScratchpadAction myScratchPads "terminal"),
    ((modMask, xK_l), namedScratchpadAction myScratchPads myNotes),
    ((modMask .|. shiftMask, xK_x), spawn "xsecurelock"),
    ((modMask .|. shiftMask, xK_j), nextScreen),
    ((modMask .|. shiftMask, xK_k), prevScreen),
    ((modMask .|. controlMask, xK_j), shiftNextScreen),
    ((modMask .|. controlMask, xK_k), shiftPrevScreen),
    ((modMask .|. shiftMask, xK_s), swapNextScreen)
  ]

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm defaultFloating,
    NS myNotes spawnNotes findNotes defaultFloating
  ]
  where
    spawnTerm = myTerminal ++ " -T=scratchpad"
    findTerm = title =? "scratchpad"
    spawnNotes = myNotes
    findNotes = appName =? myNotes

main :: IO ()
main =
  xmonad $
    ewmh $
      ewmhFullscreen $
        withEasySB
          (xmobarTop <> xmobarBottom)
          defToggleStrutsKey
          def
            { modMask = myModMask,
              terminal = myTerminal,
              workspaces = myWorkspaces,
              borderWidth = myBorderWidth,
              normalBorderColor = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
              manageHook = myManageHook <+> manageSpawn,
              startupHook = myStartupHook,
              layoutHook = myLayout
            }
          `additionalKeys` myKeys myModMask
