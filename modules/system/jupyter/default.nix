{ config, pkgs, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  users = {
    users = {
      jupyter = {
        group = "jupyter";
        isSystemUser = true;
      };
    };
    groups.jupyter = {};
  };

  services.jupyter = rec {
    enable = true;
    user = "ben";
    notebookDir = "/home/${user}";
    group = "jupyter";
    package = pkgs.python310Packages.jupyterlab;
    password = passCmd "jupyter-x1-2021";
    command = "jupyter-lab";
  };
}
