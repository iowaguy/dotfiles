{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    browserpass
  ];

  # access pass from firefox addon
  programs.browserpass.enable = true;

  programs.firefox =  {
    enable = true;
    profiles.ben = {
      isDefault = true;
      extensions = import ./extensions.nix {inherit config;};
      settings = {
        # Attribute set of Firefox preferences. Change in "about:config"
        # Documentation for options: https://kb.mozillazine.org/About:config_entries

        # newtabs as blank pages
        "browser.newtabpage.enabled" = false;
        "browser.urlbar.placeholderName" = "DuckDuckGo";
        "browser.urlbar.placeholderName.private" = "DuckDuckGo";

        # set "Do Not Track" to always
        "privacy.donottrackheader.enabled" = true;
        "privacy.donottrackheader.value" = 1;

        # opt out of studies
        "app.shield.optoutstudies.enabled" = false;

        "browser.safebrowsing.malware.enabled" = false;
        "browser.search.hiddenOneOffs" =
          "Google,Yahoo,Bing,Amazon.com,Twitter";
        "browser.search.suggest.enabled" = false;
        "browser.search.defaultenginename" = "DuckDuckGo";
        "browser.send_pings" = false;
        "browser.tabs.closeWindowWithLastTab" = true;
        "browser.urlbar.speculativeConnect.enabled" = false;
        "dom.battery.enabled" = false;
        "dom.event.clipboardevents.enabled" = true;
        "experiments.activeExperiment" = false;
        "experiments.enabled" = false;
        "experiments.supported" = false;
        "extensions.pocket.enabled" = true;
        "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
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
        "signon.rememberSignons" = false;
        "browser.startup.blankWindow" = true;
        "browser.bookmarks.defaultLocation" = "unfiled";
        "browser.bookmarks.editDialog.confirmationHintShowCount" = 3;
        "browser.bookmarks.restore_default_bookmarks" = false;
        "browser.bookmarks.showMobileBookmarks" = false;
        "browser.newtab.privateAllowed" = true;
        "browser.sessionstore.warnOnQuit" = true;
        "browser.toolbars.bookmarks.visibility" = "always";
        "extensions.formautofill.addresses.enabled" =false;
        "extensions.formautofill.addresses.usage.hasEntry" = true;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.firstTimeUse" = false;
        "privacy.trackingprotection.enabled" = true;
        "reader.color_scheme" = "dark";
        "services.sync.username" = "benweintraub34@gmail.com";
        "svg.context-properties.content.enabled" = true;
      };
    };
  };
}
