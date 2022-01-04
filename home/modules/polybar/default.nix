{ mainBar, openCalendar, config, pkgs, ... }:

let
  homedir = builtins.getEnv "HOME";
  mypolybar = pkgs.polybar.override {
    i3GapsSupport = true;
    alsaSupport = true;
    i3Support = true;
    pulseSupport = true;
  };
in {
  services.polybar = {
    enable = true;
    package = mypolybar;
    script = ''
      polybar bottom &
      polybar top &
    '';
    config = {
      "color" = import ./color.nix;
      "common" = import ./common.nix;
      "bar/top" = import ./top.nix;
      "bar/bottom" = import ./bottom.nix;
      "module/i3" = import ./modules/i3.nix;
      "module/battery" = import ./modules/battery.nix;
      "module/cpu" = import ./modules/cpu.nix;
      "module/date" = import ./modules/date.nix;
      "module/memory" = import ./modules/memory.nix;
      "module/temperature" = import ./modules/temperature.nix;
      "module/pulseaudio" = import ./modules/pulseaudio.nix;
    };
  };
}
