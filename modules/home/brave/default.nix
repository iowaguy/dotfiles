{ pkgs, config, ... }:

{
  # UI scale comes from Xft.dpi / services.xserver.dpi on the machine (see
  # machines/x1-2021/machine.nix). No --force-device-scale-factor so Chromium
  # keeps following that DPI after login.
  programs.chromium = {
    enable = true;
    package = pkgs.brave;
    extensions = [
      { id = "ldipcbpaocekfooobnbcddclnhejkcpn"; } # Google Scholar button
      { id = "pehlnokhmjhnlghjkjbepjimjbcnjnlb"; } # Cookbook app
      { id = "nkbihfbeogaeaoehlefnkodbefgpgknn"; } # MetaMask
      { id = "ekhagklcjbdpajgpjgmbionohlpdbjgc"; } # Zotero Connector
      { id = "dhdgffkkebhmkfjojejmpbldmpobfkfo"; } # Tampermonkey
      { id = "chphlpgkkbolifaimnlloiipkdnihall"; } # OneTab
    ];
  };

  programs.zsh.localVariables.BROWSER = "brave";

  # Add session variable for KDE
  home.sessionVariables = {
    BROWSER = "brave";
    KDE_FULL_SESSION = "true";
    KDE_SESSION_VERSION = "6";
  };

  home.file.".local/share/applications/zoom-handler.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${config.home.profileDirectory}/bin/zoom
    Name=zoom
  '';
  xdg = {
    mimeApps = {
      associations.added = {
        "x-scheme-handler/http" = [ "brave-browser.desktop" ];
        "x-scheme-handler/https" = [ "brave-browser.desktop" ];
        "x-scheme-handler/zoomus" = [ "zoom-handler.desktop" ];
        "x-scheme-handler/zoommtg" = [ "zoom-handler.desktop" ];
        "text/html" = [ "brave-browser.desktop" ];
      };
      defaultApplications = {
        "x-scheme-handler/http" = [ "brave-browser.desktop" ];
        "x-scheme-handler/https" = [ "brave-browser.desktop" ];
        "x-scheme-handler/zoomus" = [ "zoom-handler.desktop" ];
        "x-scheme-handler/zoommtg" = [ "zoom-handler.desktop" ];
        "text/html" = [ "brave-browser.desktop" ];
      };
    };
  };
}
