{
  networking = {
    firewall.allowedTCPPorts = [
      22067 # syncthing
      36885 # syncthing
    ];
    firewall.allowedUDPPorts = [
      36885 # syncthing
    ];
  };

  services.syncthing = {
    ## Uncomment this for remote debugging
    #guiAddress = "0.0.0.0:8384";
    # enable = true;
    enable = false;
    user = "ben";
    dataDir = "/home/ben";
    settings = {
      folders = {
        "/home/ben/projects" = {
          id = "projects";
          devices = [
            "x1-2021"
            "acadia"
          ];
        };
        "/home/ben/areas" = {
          id = "areas";
          devices = [
            "x1-2021"
            "acadia"
          ];
        };
        "/home/ben/resources" = {
          id = "resources";
          devices = [
            "x1-2021"
            "acadia"
          ];
        };
        "/home/ben/archive" = {
          id = "archive";
          devices = [
            "x1-2021"
            "acadia"
          ];
        };
        "/home/ben/.gnupg" = {
          id = "gpg";
          devices = [
            "x1-2021"
            "acadia"
          ];
        };
      };
      devices = {
        x1-2021 = {
          id = "KGTWRR3-4U5RNPZ-JA5HNTG-S7UHBKS-54OYYS5-O4O4NRW-LTQRHJQ-K44XOQG";
        };
        acadia = {
          id = "NKYLM6Y-ESOB3LX-I46FCEL-RRLREZG-KXZPCKW-6ECKMD2-DRV6PYB-3BMFWAF";
        };
      };
    };
  };
}
