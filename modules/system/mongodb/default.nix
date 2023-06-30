{ pkgs, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  # Used in frontrunning project. Enable when needed.
  services.mongodb = {
    enable = false;
    bind_ip = "0.0.0.0";
    enableAuth = true;
    initialRootPassword = passCmd "mongodb-root-isec-desktop";
    package = pkgs.mongodb-5_0;
  };
}
