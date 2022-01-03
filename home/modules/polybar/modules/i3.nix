{
  type = "internal/i3";
  pin-workspaces = true;
  index-sort = true;

  # Use fuzzy (partial) matching on labels when assigning
  # icons to workspaces
  # Example: code#♚ will apply the icon to all workspaces
  # containing 'code' in the label
  # Default: false
  #fuzzy-match = true
  label-separator = "|";
  label-separator-padding = 1;
  label-separator-foreground = "#ffb52a";
  label-dimmed-underline = "\${common.background}";

  # Available tags:
  #   <label-state> (default) - gets replaced with <label-(focused|unfocused|visible|urgent)>
  #   <label-mode> (default)
  format = "<label-state> <label-mode>";

  # Available tokens:
  #   %mode%
  # Default: %mode%
  label-mode = "%mode%";
  label-mode-padding = 0;
  label-mode-background = "#e60053";

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  #label-focused =  %index%
  label-focused-foreground = "#ffffff";
  label-focused-background = "#3f3f3f";
  label-focused-underline = "#fba922";
  label-focused-padding = 0;

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  #label-unfocused = %index%
  label-unfocused-padding = 0;

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  #label-visible = %index%
  label-visible-underline = "#555555";
  label-visible-padding = 0;

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  #label-urgent = %index%
  label-urgent-foreground = "#000000";
  label-urgent-background = "#bd2c40";
  label-urgent-padding = 0;
}
