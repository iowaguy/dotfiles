{ pkgs, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  home.packages = with pkgs; [
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
