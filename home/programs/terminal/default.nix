{ pkgs, ... }:

{
  home.packages = with pkgs; [
    inconsolata  # font
  ];

  programs.kitty = {
    enable = true;
    settings = {
      enable_audio_bell = false;
      scrollback_lines = 10000;
      strip_trailing_spaces = "smart";
      background_opacity = "0.9";
    };
  };
}
