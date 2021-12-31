{
  services.syncthing = {
    enable = true;
    user = "ben";
    dataDir = "/home/ben";
    folders = {
      "/home/ben/workspace" = {
        id = "workspace";
        devices = [
          "ben-isec"
          "kansas"
        ];
      };
      "/home/ben/.gnupg" = {
        id = "gpg";
        devices = [
          "ben-isec"
          "kansas"
        ];
      };
    };
    devices = {
      kansas = {
        id = "RXSK5VH-XDVDTLZ-7ULY4OI-OZPI6UQ-ONFWQS3-FJQZ7JB-G4CFXC4-ARV4XQH";
      };
      ben-isec = {
        id = "2JZNKVK-4U4JAQD-V32XHXX-Y5454TO-PNDBI3K-WO7UZFD-VUUXN4H-23GSPQW";
      };
    };
  };
}
