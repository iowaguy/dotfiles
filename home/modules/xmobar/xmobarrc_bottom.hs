Config
  {
    font = "Iosevka Comfy Bold 25",
    -- position = Bottom,
    position = BottomH 30,
  -- , position = BottomP 0 800 -- align left, with 800 pixel space on the right for the tray
  -- , position = Static { xpos = 0, ypos = 0, width = 1346, height = 20 }
    template = "%XMonadLog2%}{",
    commands =
    [
      Run NamedXPropertyLog "_XMONAD_LOG_2" "XMonadLog2"
    ]
}
