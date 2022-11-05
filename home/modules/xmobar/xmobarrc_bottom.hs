Config
  {
    font = "xft:iosevka:size=11:bold:antialias=true"
  , position = Bottom
  -- , position = BottomP 0 800 -- align left, with 800 pixel space on the right for the tray
  -- , position = Static { xpos = 0, ypos = 0, width = 1346, height = 20 }
  -- , template = "%XMonadLog%}{"
  , template = "%XMonadLog2%}{"
  , commands =
    [
      Run NamedXPropertyLog "_XMONAD_LOG_2" "XMonadLog2"
    ]
}
