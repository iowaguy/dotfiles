{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    browserpass
  ];

  # access pass from firefox addon
  programs.browserpass.enable = true;

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
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
      tridactyl
      (buildFirefoxXpiAddon {
        pname = "zotero-connector";
        version = "5.0.60";
        addonId = "zotero@chnm.gmu.edu";
        url = "https://download.zotero.org/connector/firefox/release/Zotero_Connector-5.0.60.xpi";
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
      (buildFirefoxXpiAddon {
        pname = "unhook-youtube";
        version = "1.4.2";
        addonId = "myallychou@gmail.com";
        url = "https://addons.mozilla.org/firefox/downloads/file/3727210/unhook_remove_youtube_recommended_videos_comments-1.4.2-an+fx.xpi";
        sha256 = "1j3v054hh6lplaryn60fq9s6gwm0zxlbcsxzqh17mkb4p1v4281j";
        meta = {};
      })
      (buildFirefoxXpiAddon {
        pname = "bib-it-now";
        version = "0.899";
        addonId = "bibitnow018@aqpl.mc2.chalmers.se";
        url = "https://addons.mozilla.org/firefox/downloads/file/3674294/bibitnow-0.899-fx.xpi";
        sha256 = "0jvy8bjdqp29w73ksqq180ljx7pfsfyi9rqml7006r7a0amhlbkq";
        meta = {};
      })
      (buildFirefoxXpiAddon {
        pname = "adblocker-ultimate";
        version = "3.7.10";
        addonId = "adblockultimate@adblockultimate.net";
        url = "https://addons.mozilla.org/firefox/downloads/file/3687310/adblocker_ultimate-3.7.10-an+fx.xpi";
        sha256 = "0xyr8ihdrr40f9sk75h5cknm6xy6wpdphdbzfzqg0vxh7fr7q6kf";
        meta = {};
      })
      (buildFirefoxXpiAddon {
        pname = "disable-facebook-news-feed";
        version = "2.1";
        addonId = "	{85cd2b5d-b3bd-4037-8335-ced996a95092}";
        url = "https://addons.mozilla.org/firefox/downloads/file/3622480/disable_facebook_news_feed-2.1-an+fx.xpi";
        sha256 = "1dxdwx9hab3pjg9grp2n22gypq47pz050mh2di8p5p1aqi2q9wki";
        meta = {};
      })
      (buildFirefoxXpiAddon {
        pname = "google-scholar-button";
        version = "3.1";
        addonId = "button@scholar.google.com";
        url = "https://addons.mozilla.org/firefox/downloads/file/3656589/google_scholar_button-3.1-fx.xpi";
        sha256 = "11v1zzp06brv6nilhf0qbqy5y8xqb997sa2by3s16mmn87903iyf";
        meta = {};
      })
      (buildFirefoxXpiAddon {
        pname = "one-tab";
        version = "1.54";
        addonId = "extension@one-tab.com";
        url = "https://addons.mozilla.org/firefox/downloads/file/3648014/onetab-1.39-an+fx.xpi";
        sha256 = "0ian4ag52l0ysp7qf2nc2z7v76xddb9vx2f643nhd6j2gclm78b4";
        meta = {};
      })
    ];

    profiles.ben = {
     isDefault = true;
     settings = {
       # Attribute set of Firefox preferences.

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
       "browser.send_pings" = false;
       "browser.tabs.closeWindowWithLastTab" = true;
       "browser.urlbar.speculativeConnect.enabled" = false;
       "dom.battery.enabled" = false;
       "dom.event.clipboardevents.enabled" = true;
       "experiments.activeExperiment" = false;
       "experiments.enabled" = false;
       "experiments.supported" = false;
       "extensions.pocket.enabled" = false;
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
       "signon.rememberSignons" = true;
       "browser.startup.blankWindow" = true;
       "browser.bookmarks.defaultLocation" = "unfiled";
       "browser.bookmarks.editDialog.confirmationHintShowCount" = 3;
       "browser.bookmarks.restore_default_bookmarks" = false;
       "browser.bookmarks.showMobileBookmarks" = true;
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
     };
    };
  };
}
