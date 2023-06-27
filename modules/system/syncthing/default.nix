{
  services.syncthing = {
    ## Uncomment this for remote debugging
    #guiAddress = "0.0.0.0:8384";
    enable = true;
    user = "ben";
    dataDir = "/home/ben";
    folders = {
      "/home/ben/workspace" = {
        id = "workspace";
        devices = [
          "ben-isec"
          "kansas"
          "x1-2021"
        ];
      };
      "/home/ben/.gnupg" = {
        id = "gpg";
        devices = [
          "ben-isec"
          "kansas"
          "x1-2021"
        ];
      };
    };
    devices = {
      x1-2021 = {
        id = "KGTWRR3-4U5RNPZ-JA5HNTG-S7UHBKS-54OYYS5-O4O4NRW-LTQRHJQ-K44XOQG";
      };
      kansas = {
        id = "RXSK5VH-XDVDTLZ-7ULY4OI-OZPI6UQ-ONFWQS3-FJQZ7JB-G4CFXC4-ARV4XQH";
      };
      ben-isec = {
        id = "2JZNKVK-4U4JAQD-V32XHXX-Y5454TO-PNDBI3K-WO7UZFD-VUUXN4H-23GSPQW";
      };
    };
  };
}
