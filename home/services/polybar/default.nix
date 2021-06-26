{ mainBar, openCalendar, config, pkgs, ... }:

let
  homedir = builtins.getEnv "HOME";
in
{
  services.polybar = {
    enable = true;
    script = ''
      polybar laptop & disown
      # polybar tb1 &
      # polybar tb2 &
      # polybar hdmi &
    '';

    config = let
        barconf = monitor: {
          monitor = "\${env:MONITOR:${monitor}}";
          width = "100%";
          height = "3%";
          radius = 0;
          modules-left = "workspaces-xmonad title-xmonad";
          modules-center = "date";
          modules-right = "cpu memory battery backlight pulseaudio";
          font-0 = "FuraMono Nerd Font Mono:size=10";
          label-active-font = 0;
        };
     in {
      "bar/laptop" = barconf "eDP";
      # "bar/tb1" = barconf "DP1-1";
      # "bar/tb2" = barconf "DP1-2";
      # "bar/hdmi" = barconf "HDMI1";
      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%m.%d.%y";
        time = "%A %I:%M%P";
        label = "%time%  %date%";
      };
      "module/battery" = {
        type = "internal/battery";
        # This is useful in case the battery never reports 100% charge
        full-at = 94;
        # Use the following command to list batteries and adapters:
        # $ ls -1 /sys/class/power_supply/
        battery = "BAT0";
        adapter = "AC";
        # see "man date" for details on how to format the time string
        # NOTE: if you want to use syntax tags here you need to use %%{...}
        # Default: %H:%M:%S
        time-format = " %H:%M ";
        #   <label-charging> (default)
        #   <bar-capacity>
        #   <ramp-capacity>
        #   <animation-charging>
        format-charging = "| <animation-charging><label-charging> ";

        # Available tags:
        #   <label-discharging> (default)
        #   <bar-capacity>
        #   <ramp-capacity>
        #   <animation-discharging>
        format-discharging = "| <ramp-capacity><label-discharging> ";

        # Available tags:
        #   <label-full> (default)
        #   <bar-capacity>
        #   <ramp-capacity>
        format-full = "| <ramp-capacity><label-full> ";

        # Available tokens:
        #   %percentage% (default) - is set to 100 if full-at is reached
        #   %percentage_raw%
        #   %time%
        #   %consumption% (shows current charge rate in watts)
        label-charging = " Charging %percentage%% ";

        # Available tokens:
        #   %percentage% (default) - is set to 100 if full-at is reached
        #   %percentage_raw%
        #   %time%
        #   %consumption% (shows current discharge rate in watts)
        label-discharging = " Discharging %percentage%% ";

        # Available tokens:
        #   %percentage% (default) - is set to 100 if full-at is reached
        #   %percentage_raw%
        label-full = " Fully charged ";

        # Only applies if <ramp-capacity> is used
        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";

        # Only applies if <bar-capacity> is used
        bar-capacity-width = 10;

        # Only applies if <animation-charging> is used
        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";
        # Framerate in milliseconds
        animation-charging-framerate = 750;

        # Only applies if <animation-discharging> is used
        animation-discharging-0 = "";
        animation-discharging-1 = "";
        animation-discharging-2 = "";
        animation-discharging-3 = "";
        animation-discharging-4 = "";
        # Framerate in milliseconds
        animation-discharging-framerate = 500;
      };
      "module/cpu" = {
        type = "internal/cpu";
        # Available tags:
        #   <label> (default)
        #   <bar-load>
        #   <ramp-load>
        #   <ramp-coreload>
        format = "<label> <ramp-coreload> ";
        # Available tokens:
        #   %percentage% (default) - total cpu load averaged over all cores
        #   %percentage-sum% - Cumulative load on all cores
        #   %percentage-cores% - load percentage for each core
        #   %percentage-core[1-9]% - load percentage for specific core
        label = "| CPU %percentage%%";
        # Spacing between individual per-core ramps
        ramp-coreload-spacing = "1";
        ramp-coreload-0 = "▁";
        ramp-coreload-1 = "▂";
        ramp-coreload-2 = "▃";
        ramp-coreload-3 = "▄";
        ramp-coreload-4 = "▅";
        ramp-coreload-5 = "▆";
        ramp-coreload-6 = "▇";
        ramp-coreload-7 = "█";
      };
      "module/memory" = {
        type = "internal/memory";
        interval = 3;
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
        format = " <label> <bar-used> ";
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
        label = "| RAM %gb_used%/%gb_free%";
        # Only applies if <bar-used> is used
        bar-used-indicator = "";
        bar-used-width = "20";
        bar-used-foreground-0 = "#55aa55";
        bar-used-foreground-1 = "#557755";
        bar-used-foreground-2 = "#f5a70a";
        bar-used-foreground-3 = "#ff5555";
        bar-used-fill = "▐";
        bar-used-empty = "▐";
        bar-used-empty-foreground = "#444444";

        # Only applies if <ramp-used> is used
        ramp-used-0 = "▁";
        ramp-used-1 = "▂";
        ramp-used-2 = "▃";
        ramp-used-3 = "▄";
        ramp-used-4 = "▅";
        ramp-used-5 = "▆";
        ramp-used-6 = "▇";
        ramp-used-7 = "█";

        # Only applies if <ramp-free> is used
        ramp-free-0 = "▁";
        ramp-free-1 = "▂";
        ramp-free-2 = "▃";
        ramp-free-3 = "▄";
        ramp-free-4 = "▅";
        ramp-free-5 = "▆";
        ramp-free-6 = "▇";
        ramp-free-7 = "█";
      };
      "module/workspaces-xmonad" = {
        type = "custom/script";
        exec = "${pkgs.coreutils}/bin/tail -F /tmp/.xmonad-workspace-log";
        exec-if = "[ -p /tmp/.xmonad-workspace-log ]";
        tail = true;
      };
      "module/title-xmonad" = {
        type = "custom/script";
        exec = "${pkgs.coreutils}/bin/tail -F /tmp/.xmonad-title-log";
        exec-if = "[ -p /tmp/.xmonad-title-log ]";
        tail = true;
      };
      # "module/pulseaudio" = {
      #   type = "internal/pulseaudio";
      #   # Sink to be used, if it exists (find using `pacmd list-sinks`, name field)
      #   # If not, uses default sink
      #   sink = "alsa_output.pci-0000_00_1f.3.analog-stereo";
      #   # Use PA_VOLUME_UI_MAX (~153%) if true, or PA_VOLUME_NORM (100%) if false
      #   # Default: true
      #   use-ui-max = true;
      #   # Interval for volume increase/decrease (in percent points)
      #   interval = 5;
      #   # Available tags:
      #   #   <label-volume> (default)
      #   #   <ramp-volume>
      #   #   <bar-volume>
      #   format-volume = " <label-volume> <ramp-volume> ";
      #   # Available tags:
      #   #   <label-muted> (default)
      #   #   <ramp-volume>
      #   #   <bar-volume>
      #   #format-muted = <label-muted>
      #   # Available tokens:
      #   #   %percentage% (default)
      #   #   %decibels% (unreleased)
      #   label-volume = "| %percentage%% ";
      #   # Available tokens:
      #   #   %percentage% (default)
      #   #   %decibels% (unreleased)
      #   label-muted = "| 🔇 muted";
      #   label-muted-foreground = "#666";

      #   # Only applies if <ramp-volume> is used
      #   ramp-volume-0 = "🔈";
      #   ramp-volume-1 = "🔉";
      #   ramp-volume-2 = "🔊";

      #   # Right and Middle click (unreleased)
      #   # click-right = "${pkgs.pavucontrol}/bin/pavucontrol &";
      #  };
      # "module/backlight" ={
      #   type = "internal/backlight";
      #   # Use the following command to list available cards:
      #   # $ ls -1 /sys/class/backlight/
      #   card = "intel_backlight";
      #   # Available tags:
      #   #   <label> (default)
      #   #   <ramp>
      #   #   <bar>
      #   format = "<ramp>";

      #   # Available tokens:
      #   #   %percentage% (default)
      #   label = "%percentage%%";

      #   # Only applies if <ramp> is used
      #   ramp-0 = "🌕";
      #   ramp-1 = "🌔";
      #   ramp-2 = "🌓";
      #   ramp-3 = "🌒";
      #   ramp-4 = "🌑";

      #   # Only applies if <bar> is used
      #   bar-width = "10";
      #   bar-indicator = "|";
      #   bar-fill = "─";
      #   bar-empty = "─";
      # };
    };
  };
}
