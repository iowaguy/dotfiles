{ pkgs, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  home.packages = with pkgs; [
    afew # initial tagging for notmuch
    notmuch # an email search engine
    mailspring
  ];

  programs = {
    # email client
    thunderbird = {
      enable = true;
      profiles = {
        ben = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };
    };
  };
}
