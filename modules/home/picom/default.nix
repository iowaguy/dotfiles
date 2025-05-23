{ config, pkgs, ... }:
{
  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 1.0;
    package = pkgs.picom-next;
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    opacityRules = [
      "100:name *= 'i3lock'"
      "100:name *= 'Brave'"
      "100:name *= 'Evince'"
      "100:name *= 'Zoom'"
      "100:name *= 'Slack'"
      "100:name *= 'Teams'"
      "100:name *= 'Zotero'"
      "90:name *= 'Emacs'"
    ];
    shadow = true;
    shadowOpacity = 0.75;
    settings = {
      transparent-clipping = true;
      unredir-if-possible = true;
    };
  };
}
