{ pkgs, ... }:

with pkgs.nur.repos.rycee.firefox-addons;
[
  browserpass
  facebook-container
  link-cleaner
  refined-github
  duckduckgo-privacy-essentials
]
