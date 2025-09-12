{ pkgs, ... }:

{
  home.packages = with pkgs; [
    inconsolata  # font
  ];

  programs.alacritty = {
    enable = true;
  };

  programs.kitty = {
    enable = true;

    # TODO enable this after upgading to 23.05 or possibly the next release
    # shellIntegration.enableZshIntegration = true;

    # View all settings here: https://sw.kovidgoyal.net/kitty/conf/
    settings = {
      font_size = 25;
      enable_audio_bell = false;
      scrollback_lines = 10000;
      strip_trailing_spaces = "smart";
      background_opacity = "0.9";
    };
  };
}
