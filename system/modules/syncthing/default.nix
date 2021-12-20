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
        id = "MDCFDOP-O7O42LB-2NKDRRR-FLQ2SD7-GUZZ2HU-KIVUCZR-M2KVH2T-BQ4XRAW";
      };
      ben-isec = {
        id = "NTT2XIC-TXWJEYH-IIP75QY-WEOYMAH-JSMRTVD-WAXWPEO-TVIIF6X-VY2OVAA";
      };
    };
  };
}
