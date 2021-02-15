{ config, pkgs, lib, ... }:
{
  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      dark-night-mode
      decentraleyes
      duckduckgo-privacy-essentials
      facebook-container
      https-everywhere
      i-dont-care-about-cookies
      ipfs-companion
      link-cleaner
      linkhints
      octotree
      org-capture
      refined-github
      (buildFirefoxXpiAddon {
        pname = "zotero-connector";
        version = "5.0.60";
        addonId = "zotero@chnm.gmu.edu";
        url = " https://download.zotero.org/connector/firefox/release/Zotero_Connector-5.0.60.xpi";
        sha256 = "1c4n4rxcmf556nim2j5gwjf45ka63dr4bfy2rmbrnzfbvgrrp7hh";
        meta = {};
      })
      (buildFirefoxXpiAddon {
        pname = "1password-x-password-manager";
        version = "1.23.1";
        addonId = "{d634138d-c276-4fc8-924b-40a0ea21d284}";
        url = "https://addons.mozilla.org/firefox/downloads/file/3726657/1password_password_manager-1.23.1-fx.xpi";
        sha256 = "47e9e98f1072d93d595002dc8c221e5cca17e091b3431563a8e3e2be575c5cc1";
        meta = with lib; {
          homepage = "https://1password.com";
          description = "The best way to experience 1Password in your browser. Easily sign in to sites, generate passwords, and store secure information, including logins, credit cards, notes, and more.";
          license = {
            shortName = "1pwd";
            fullName = "Service Agreement for 1Password users and customers";
            url = "https://1password.com/legal/terms-of-service/";
            free = false;
          };
          platforms = platforms.all;
        };
      })
    ];

    enableAdobeFlash = false;
    profiles.ben = {
     isDefault = true;
     settings = {
       # Attribute set of Firefox preferences.

       # newtabs as blank pages
       "browser.newtabpage.enabled" = false;
       "browser.urlbar.placeholderName" = "DuckDuckGo";

       # set "Do Not Track" to always
       "privacy.donottrackheader.enabled" = true;
       "privacy.donottrackheader.value" = 1;

       # opt out of studies
       "app.shield.optoutstudies.enabled" = false;

       "browser.safebrowsing.malware.enabled" = false;
       "browser.search.hiddenOneOffs" =
         "Google,Yahoo,Bing,Amazon.com,Twitter";
       "browser.search.suggest.enabled" = false;
       "browser.send_pings" = false;
       "browser.tabs.closeWindowWithLastTab" = false;
       "browser.urlbar.speculativeConnect.enabled" = false;
       "dom.battery.enabled" = false;
       "dom.event.clipboardevents.enabled" = false;
       "experiments.activeExperiment" = false;
       "experiments.enabled" = false;
       "experiments.supported" = false;
       "extensions.pocket.enabled" = false;
       "general.smoothScroll" = true;
       "geo.enabled" = false;
       "gfx.webrender.all" = true;
       "layout.css.devPixelsPerPx" = "1";
       "media.navigator.enabled" = false;
       "media.video_stats.enabled" = false;
       "network.IDN_show_punycode" = true;
       "network.allow-experiments" = false;
       "network.dns.disablePrefetch" = true;
       "network.http.referer.XOriginPolicy" = 2;
       "network.http.referer.XOriginTrimmingPolicy" = 2;
       "network.http.referer.trimmingPolicy" = 1;
       "network.prefetch-next" = false;
       "permissions.default.shortcuts" = 2; # Don't steal my shortcuts!
       "privacy.firstparty.isolate" = true;
       "signon.rememberSignons" = true;
     };
    };
  };
}
