{ pkgs, pkgsUnstable, ... }:

{
  programs.chromium = {
    enable = true;
    # package = pkgs.brave;
    package = pkgsUnstable.brave;
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

  xdg.mimeApps = {
    associations.added = {
      "x-scheme-handler/http" = [ "brave-browser.desktop" ];
      "x-scheme-handler/https" = [ "brave-browser.desktop" ];
      "text/html" = [ "brave-browser.desktop" ];
    };
    defaultApplications = {
      "x-scheme-handler/http" = [ "brave-browser.desktop" ];
      "x-scheme-handler/https" = [ "brave-browser.desktop" ];
      "text/html" = [ "brave-browser.desktop" ];
    };
  };
}
