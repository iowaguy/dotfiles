Config
  {
    font = "xft:iosevka:size=11:bold:antialias=true"
  , position = Top
  -- , position = BottomP 0 800 -- align left, with 800 pixel space on the right for the tray
  -- , position = Static { xpos = 0, ypos = 0, width = 1346, height = 20 }
  , template = "}%cpu%{%alsa:default:Master% (%wlp0s20f3wi%) %battery% %date%  "
  , commands =
    [
      Run Wireless "wlp0s20f3"
      ["--template", "<ssid> <quality>%", "--"] 100
    , Run Alsa "default" "Master"
      ["--template"
      , "<status> <volume>%", "--"
      , "-O", "♪"
      , "-o", "[muted]"
      ]
    , Run Cpu
      ["--template", " <total>", "--"]
      100
    , Run BatteryP
      ["BAT0"]
      ["--template", "<acstatus>", "--"
      ,"-O", "<left>+ <fc=#aaffaa>|</fc>"
      ,"-o", "<left>- <fc=#aaffaa>|</fc>"
      ,"-P"  -- Display percent symbol with <left>
      ,"-l", "red", "-m", "blue", "-h", "green"
      ,"-i", ""
      ] 100
    , Run Date "%A, %d %B %H:%M %Z" "date" 100
    ]
}
