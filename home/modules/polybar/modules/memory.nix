{
  type = "internal/memory";

  # Seconds to sleep between updates
  # Default: 1
  interval = 1;

  # Available tags:
  #   <label> (default)
  #   <bar-used>
  #   <bar-free>
  #   <ramp-used>
  #   <ramp-free>
  #   <bar-swap-used>
  #   <bar-swap-free>
  #   <ramp-swap-used>
  #   <ramp-swap-free>
  format = "<label>";
  format-prefix = "";
  format-background = "\${color.mb}";
  format-padding = "\${layout.module-padding}";

  # Available tokens:
  #   %percentage_used% (default)
  #   %percentage_free%
  #   %gb_used%
  #   %gb_free%
  #   %gb_total%
  #   %mb_used%
  #   %mb_free%
  #   %mb_total%
  #   %percentage_swap_used%
  #   %percentage_swap_free%
  #   %mb_swap_total%
  #   %mb_swap_free%
  #   %mb_swap_used%
  #   %gb_swap_total%
  #   %gb_swap_free%
  #   %gb_swap_used%

  label = " %gb_used% / %gb_total% (%percentage_used%%)";

  # Only applies if <bar-used> is used
  ##bar-used-indicator =
  ##bar-used-width = 50
  ##bar-used-foreground-0 = #55aa55
  ##bar-used-foreground-1 = #557755
  ##bar-used-foreground-2 = #f5a70a
  ##bar-used-foreground-3 = #ff5555
  ##bar-used-fill = ▐
  ##bar-used-empty = ▐
  ##bar-used-empty-foreground = #444444

  # Only applies if <ramp-used> is used
  ##ramp-used-0 = 
  ##ramp-used-1 = 
  ##ramp-used-2 = 
  ##ramp-used-3 = 
  ##ramp-used-4 = 

  # Only applies if <ramp-free> is used
  ##ramp-free-0 = 
  ##ramp-free-1 = 
  ##ramp-free-2 = 
  ##ramp-free-3 = 
  ##ramp-free-4 = 
}
