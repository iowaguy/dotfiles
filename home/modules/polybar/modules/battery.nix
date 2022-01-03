{
  type = "internal/battery";

  # This is useful in case the battery never reports 100% charge
  full-at = 98;

  # Use the following command to list batteries and adapters:
  # $ ls -1 /sys/class/power_supply/
  battery = "BAT0";
  adapter = "ADP1";

  # If an inotify event haven't been reported in this many
  # seconds, manually poll for new values.
  #
  # Needed as a fallback for systems that don't report events
  # on sysfs/procfs.
  #
  # Disable polling by setting the interval to 0.
  #
  # Default: 5
  poll-interval = 5;

  # see "man date" for details on how to format the time string
  # NOTE: if you want to use syntax tags here you need to use %%{...}
  # Default: %H:%M:%S
  time-format = "%H:%M";

  # Available tags:
  #   <label-charging> (default)
  #   <bar-capacity>
  #   <ramp-capacity>
  #   <animation-charging>
  format-charging = "<animation-charging> <label-charging>";
  format-charging-background = "\${color.mb}";
  format-charging-padding = "\${common.module-padding}";
  ##format-charging-prefix = 
  # Available tags:
  #   <label-discharging> (default)
  #   <bar-capacity>
  #   <ramp-capacity>
  #   <animation-discharging>
  format-discharging = "<ramp-capacity> <label-discharging>";
  format-discharging-background = "\${color.mb}";
  format-discharging-padding = "\${common.module-padding}";

  # Available tags:
  #   <label-full> (default)
  #   <bar-capacity>
  #   <ramp-capacity>
  #format-full = <ramp-capacity> <label-full>

  # Available tokens:
  #   %percentage% (default)
  #   %time%
  #   %consumption% (shows current charge rate in watts)

  label-charging = "%percentage%%";

  # Available tokens:
  #   %percentage% (default)
  #   %time%
  #   %consumption% (shows current discharge rate in watts)
  label-discharging = "%percentage%%";

  # Available tokens:
  #   %percentage% (default)
  label-full = "Fully Charged";
  label-full-background = "\${color.mb}";
  label-full-padding = "\${common.module-padding}";

  # Only applies if <ramp-capacity> is used
  ramp-capacity-0 = "";
  ramp-capacity-1 = "";
  ramp-capacity-2 = "";
  ramp-capacity-3 = "";
  ramp-capacity-4 = "";
  ramp-capacity-5 = "";
  ramp-capacity-6 = "";
  ramp-capacity-7 = "";
  ramp-capacity-8 = "";
  ramp-capacity-9 = "";

  # Only applies if <bar-capacity> is used
  #bar-capacity-width = 10

  # Only applies if <animation-charging> is used
  animation-charging-0 = "";
  animation-charging-1 = "";
  animation-charging-2 = "";
  animation-charging-3 = "";
  animation-charging-4 = "";
  animation-charging-5 = "";
  animation-charging-6 = "";
  animation-charging-7 = "";
  animation-charging-8 = "";

  # Framerate in milliseconds
  animation-charging-framerate = 750;

  # Only applies if <animation-discharging> is used
  ##animation-discharging-0 = "";
  ##animation-discharging-1 = "";
  ##animation-discharging-2 = "";
  ##animation-discharging-3 = "";
  ##animation-discharging-4 = "";
  ##animation-discharging-5 = "";
  ##animation-discharging-6 = "";
  ##animation-discharging-7 = "";
  ##animation-discharging-8 = "";

  # Framerate in milliseconds
  #animation-discharging-framerate = 500;
}
