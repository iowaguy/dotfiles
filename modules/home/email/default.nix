{ pkgs, pkgsUnstable, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  programs = {
    # email client
    thunderbird = {
      package = pkgsUnstable.thunderbird;
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
