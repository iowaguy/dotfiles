let
  pkgs = import (import ../../../../nix/sources.nix).nixpkgs {};
in {
  type = "custom/script";
  # type = "internal/xworkspaces";
  # exec = "${pkgs.xmonad-log}/bin/xmonad-log";
  tail = true;
  exec = "tail -F /tmp/.xmonad-workspace-log";
  exec-if = "[ -p /tmp/.xmonad-workspace-log ]";
  # pin-workspaces = true;
  # index-sort = true;

  enable-scroll = false;

  # Use fuzzy (partial) matching on labels when assigning
  # icons to workspaces
  # Example: code#♚ will apply the icon to all workspaces
  # containing 'code' in the label
  # Default: false
  #fuzzy-match = true
  label-separator = "|";
  label-separator-padding = 0;
  label-separator-foreground = "\${color.amber}";
  label-dimmed-underline = "\${common.background}";

  # Available tags:
  #   <label-state> (default) - gets replaced with <label-(focused|unfocused|visible|urgent)>
  #   <label-mode> (default)
  # format = "<label-state> <label-mode>";
  format = "<label-state>";

  # Available tokens:
  #   %mode%
  # Default: %mode%
  label-mode = "%mode%";
  label-mode-padding = 0;
  label-mode-background = "\${color.pishade5}";

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  label-focused = " %name% ";
  label-focused-foreground = "\${color.white}";
  label-focused-background = "\${color.greshade2}";
  label-focused-underline = "\${color.amber}";
  label-focused-padding = 0;

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  label-unfocused = " %name% ";
  label-unfocused-padding = 0;

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  label-visible = " %name% ";
  label-visible-underline = "\${color.greshade3}";
  label-visible-padding = 0;

  # Available tokens:
  #   %name%
  #   %icon%
  #   %index%
  #   %output%
  # Default: %icon%  %name%
  label-urgent = " %name% ";
  label-urgent-foreground = "\${color.black}";
  label-urgent-background = "\${color.reshade1}";
  label-urgent-padding = 0;
}
