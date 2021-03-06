{
  services.picom = {
    enable = true;
    activeOpacity = "1.0";
    inactiveOpacity = "1.0";
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    opacityRule = [
      "100:name *= 'i3lock'"
      "100:name *= 'Firefox'"
      "100:name *= 'Evince'"
      "100:name *= 'Zoom'"
      "100:name *= 'Slack'"
      "100:name *= 'Teams'"
      "100:name *= 'Zotero'"
      "90:name *= 'Alacritty'"
    ];
    shadow = true;
    shadowOpacity = "0.75";
  };
}
