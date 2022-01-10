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
        id = "NXBYQWZ-A7BPVNU-2V2ZGFN-77XL7WE-4MI2U6N-DUWE3Q7-33N4XG4-UDSGKA6";
      };
      ben-isec = {
        id = "2JZNKVK-4U4JAQD-V32XHXX-Y5454TO-PNDBI3K-WO7UZFD-VUUXN4H-23GSPQW";
      };
    };
  };
}
