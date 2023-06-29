{ config, ... }:

with config.nur.repos.rycee.firefox-addons;
[
  browserpass
  facebook-container
  i-dont-care-about-cookies
  link-cleaner
  octotree
  refined-github
  tridactyl
  duckduckgo-privacy-essentials
  (buildFirefoxXpiAddon {
    pname = "BitItNow";
    version = "0.902";
    addonId = "schuleje@ok.de";
    url = "https://addons.mozilla.org/firefox/downloads/file/3769373/bibitnow-0.902-fx.xpi";
    sha256 = "17y6j2gabxpjbhhmsfcg5nrrvdlzhx01vyvdn7bdw1qfhw15qr6m";
    meta = {};
  })
  (buildFirefoxXpiAddon {
    pname = "zotero-connector";
    version = "5.0.60";
    addonId = "zotero@chnm.gmu.edu";
    url = "https://download.zotero.org/connector/firefox/release/Zotero_Connector-5.0.60.xpi";
    sha256 = "1c4n4rxcmf556nim2j5gwjf45ka63dr4bfy2rmbrnzfbvgrrp7hh";
    meta = {};
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
]
