{
  type = "internal/battery";

  # This is useful in case the battery never reports 100% charge
  full-at = 98;

  # Use the following command to list batteries and adapters:
  # $ ls -1 /sys/class/power_supply/
  battery = "BAT0";
  adapter = "ADP1";

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

  # Available tags:
  #   <label-discharging> (default)
  #   <bar-capacity>
  #   <ramp-capacity>
  #   <animation-discharging>
  format-discharging = "<ramp-capacity> <label-discharging>";

  # Available tags:
  #   <label-full> (default)
  #   <bar-capacity>
  #   <ramp-capacity>
  format-full = "<label-full>";
  format-full-prefix = "";

  # Available tokens:
  #   %percentage% (default) - is set to 100 if full-at is reached
  #   %percentage_raw%
  #   %time%
  #   %consumption% (shows current charge rate in watts)
  label-charging = "%percentage%%";

  # Available tokens:
  #   %percentage% (default) - is set to 100 if full-at is reached
  #   %percentage_raw%
  #   %time%
  #   %consumption% (shows current discharge rate in watts)
  label-discharging = "%percentage%%";

  # Available tokens:
  #   %percentage% (default) - is set to 100 if full-at is reached
  #   %percentage_raw%
  label-full = "Fully charged";

  # Only applies if <ramp-capacity> is used
  animation-charging-0 = "";
  animation-charging-1 = "";
  animation-charging-2 = "";
  animation-charging-3 = "";
  animation-charging-4 = "";
  animation-charging-5 = "";
  animation-charging-6 = "";
  animation-charging-7 = "";
  animation-charging-8 = "";
  animation-charging-9 = "";

  # Only applies if <ramp-capacity> is used
  ramp-capacity-0 = "";
  ramp-capacity-1 = "";
  ramp-capacity-2 = "";
  ramp-capacity-3 = "";
  ramp-capacity-4 = "";
  ramp-capacity-5 = "";
  ramp-capacity-6 = "";
  ramp-capacity-7 = "";
  ramp-capacity-8 = "";
  ramp-capacity-9 = "";
  ramp-capacity-10 = "";
  # ramp-capacity-11 = "";

  # Framerate in milliseconds
  ramp-capacity-framerate = 750;

  # # Only applies if <animation-discharging> is used
  # animation-discharging-0 = "";
  # animation-discharging-1 = "";
  # animation-discharging-2 = "";
  # animation-discharging-3 = "";
  # animation-discharging-4 = "";
  # animation-discharging-5 = "";
  # animation-discharging-6 = "";
  # animation-discharging-7 = "";
  # animation-discharging-8 = "";
  # animation-discharging-9 = "";

  # # Framerate in milliseconds
  # animation-discharging-framerate = 500;
}
