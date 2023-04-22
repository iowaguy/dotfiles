Config
  {
    font = "Iosevka Comfy Bold 25",
    -- position = BottomSize L 70 40,
    position = BottomH 40,
    lowerOnStart = True,
    allDesktops = True,
    template = "%XMonadLog2%}{",
    commands =
    [
      Run NamedXPropertyLog "_XMONAD_LOG_2" "XMonadLog2"
    ]
}
