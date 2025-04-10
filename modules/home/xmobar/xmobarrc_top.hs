Config
  {
    font = "Iosevka Comfy Bold 25",
    position = TopSize L 91 40,
    -- position = TopH 40,
  -- , template = "}%cpu%  %memory% %alsa:default:Master% %bright% {(%wlp0s20f3wi%) %battery% %date%"
    template = "%date%} %battery% %cpu% %memory% %bright% %alsa:default:Master% (%wlp0s20f3wi%) <fc=#aaffaa>|</fc> <action=`/home/ben/.bin/pomodoro-control` button=1>%pomodoro%</action> {",
    commands =
    [
      Run Alsa "default" "Master"
      ["--template",
       "<status> <volume>%", "--",
       "-O", "♪",
       "-o", "[muted]"
      ],
      Run Cpu
      ["--template", "🧠<total>%", "--"]
      100,
      Run Memory [ "-t", "  <usedratio>%", "-d", "1", "--", "--scale", "1024"] 100,
      Run Wireless "wlp0s20f3"
      ["--template", "<ssid> <quality>%", "--"] 100,
      Run BatteryP
      ["BAT0"]
      ["--template", "<acstatus>", "--",
       "-O", "<left>+ <fc=#aaffaa>|</fc>",
       "-o", "<left>- <fc=#aaffaa>|</fc>",
       "-P",  -- Display percent symbol with <left>
       "-l", "red", "-m", "blue", "-h", "green",
       "-i", "<left> <fc=#aaffaa>|</fc>"
      ] 100,
      Run Date "%H%M | %m/%d/%y (%a)" "date" 100,
      -- Run Date "%H%M %Z | %d %B (%A)" "date" 100,
      Run Brightness ["--template", "💡<percent>%",
                      "--", "-D", "intel_backlight"
                     ] 100,

      Run NamedXPropertyLog "_XMONAD_LOG_2" "XMonadLog2",
      Run ComX "cat" ["/home/ben/.bin/pomodoro-status.txt"] "Pomodoro not running" "pomodoro" 9 -- refresh every 0.9 seconds
    ]
}
