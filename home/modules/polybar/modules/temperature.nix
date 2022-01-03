{
  type = "internal/temperature";

  # Seconds to sleep between updates
  # Default: 1
  interval = 1;

  # Thermal zone to use
  # To list all the zone types, run
  # $ for i in /sys/class/thermal/thermal_zone*# do echo "$i: $(<$i/type)"# done
  # Default: 0
  # 1 = GEN1
  thermal-zone = 1;

  #dell_smm-virtual-0
  # Full path of temperature sysfs path
  # Use `sensors` to find preferred temperature source, then run
  # $ for i in /sys/class/hwmon/hwmon*/temp*_input# do echo "$(<$(dirname $i)/name): $(cat ${i%_*}_label 2>/dev/null || echo $(basename ${i%_*})) $(readlink -f $i)"# done
  # to find path to desired file
  # Default reverts to thermal zone setting
  ##hwmon-path = /sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input
  # Threshold temperature to display warning label (in degrees celsius)
  # Default: 80
  #
  warn-temperature = 80;

  # Whether or not to show units next to the temperature tokens (°C, °F)
  # Default: true
  units = true;

  # Available tags:
  #   <label> (default)
  #   <ramp>
  format = "\<ramp> <label>";
  format-background = "\${color.mb}";
  format-padding = "\${common.module-padding}";

  # Available tags:
  #   <label-warn> (default)
  #   <ramp>
  format-warn = "<ramp> <label-warn>";
  format-warn-background = "\${color.mb}";
  format-warn-padding = "\${common.module-padding}";

  # Available tokens:
  #   %temperature% (deprecated)
  #   %temperature-c%   (default, temperature in °C)
  #   %temperature-f%   (temperature in °F)
  label = "%temperature-f%";

  # Available tokens:
  #   %temperature% (deprecated)
  #   %temperature-c%   (default, temperature in °C)
  #   %temperature-f%   (temperature in °F)
  label-warn = "%temperature-f%";
  label-warn-foreground = "#f00";

  # Requires the <ramp> tag
  # The icon selection will range from 0 to `warn-temperature`
  # with the current temperature as index.
  ramp-0 = "";
  ramp-1 = "";
  ramp-2 = "";
  ramp-3 = "";
  ramp-4 = "";
  ##ramp-foreground = #55
}
