{
  type = "internal/cpu";

  # Seconds to sleep between updates
  # Default: 1
  interval = 1;

  warn-percentage = 20;

  # Available tags:
  #   <label> (default)
  #   <bar-load>
  #   <ramp-load>
  #   <ramp-coreload>
  #format = <label> <ramp-coreload>
  format = "<label>";
  format-prefix = "  ";
  format-background = "\${color.mb}";
  format-foreground = "\${color.green}";
  format-padding = "\${common.module-padding}";

  format-warn = "<label>";
  format-warn-prefix = "  ";
  format-warn-background = "\${color.mb}";
  format-warn-foreground = "\${color.red}";
  format-warn-padding = "\${common.module-padding}";

  # Available tokens:
  #   %percentage% (default) - total cpu load averaged over all cores
  #   %percentage-sum% - Cumulative load on all cores
  #   %percentage-cores% - load percentage for each core
  #   %percentage-core[1-9]% - load percentage for specific core
  label = "%percentage%%";

  label-warn = "%percentage%%";
  label-warn-foreground = "\${color.red}";

  # Spacing between individual per-core ramps
  # ramp-coreload-spacing = 1;
  # ramp-coreload-0 = "▁";
  # ramp-coreload-1 = "▂";
  # ramp-coreload-2 = "▃";
  # ramp-coreload-3 = "▄";
  # ramp-coreload-4 = "▅";
  # ramp-coreload-5 = "▆";
  # ramp-coreload-6 = "▇";
  # ramp-coreload-7 = "█";
}
