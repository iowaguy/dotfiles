{
  type = "internal/date";

  # Seconds to sleep between updates
  interval = 1;

  # See "http://en.cppreference.com/w/cpp/io/manip/put_time" for details on how to format the date string
  # NOTE: if you want to use syntax tags here you need to use %%{...}
  ##date = %Y-%m-%d%

  # Optional time format
  # time = "| %a %d %b %Y  %H:%M";
  time = "%a %D  %H:%M";

  # if `date-alt` or `time-alt` is defined, clicking
  # the module will toggle between formats
  ##date-alt = %A, %d %B %Y

  # Available tags:
  #   <label> (default)
  format = "<label>";
  format-background = "\${color.mb}";
  format-padding = "\${common.module-padding}";
  # Available tokens:
  #   %date%
  #   %time%
  # Default: %date%
  label = "%time%";
}
