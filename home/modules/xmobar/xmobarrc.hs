Config
  {
    font = "xft:iosevka:size=11:bold:antialias=true"
  , position = Bottom
  , template    = "%XMonadLog%}{(%wlp0s20f3wi%) %battery% %date%  "
  , commands =
    [
      Run XMonadLog
    , Run Wireless "wlp0s20f3"
      ["--template", "<ssid> <quality>%", "-p", "--"] 100
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
