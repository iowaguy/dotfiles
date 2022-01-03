{
  type = "internal/pulseaudio";

  sink = "alsa_output.usb-Focusrite_Scarlett_2i4_USB-00.analog-surround-40";

  format-volume = "<label-volume> <bar-volume>";
  label-volume = "VOL %percentage%%";
  # label-volume-foreground = "\${color.mb}";
  # label-volume-background = "\${color.mb}";

  label-muted = "🔇 muted";
  label-muted-foreground = "#666";

  bar-volume-width = 10;
  bar-volume-foreground-0 = "#55aa55";
  bar-volume-foreground-1 = "#55aa55";
  bar-volume-foreground-2 = "#55aa55";
  bar-volume-foreground-3 = "#55aa55";
  bar-volume-foreground-4 = "#55aa55";
  bar-volume-foreground-5 = "#f5a70a";
  bar-volume-foreground-6 = "#ff5555";
  bar-volume-gradient = false;
  bar-volume-indicator = "|";
  bar-volume-indicator-font = 2;
  bar-volume-fill = "─";
  bar-volume-fill-font = 5;
  bar-volume-empty = "─";
  bar-volume-empty-font = 5;
  bar-volume-empty-foreground = "\${color.brown}";
}
