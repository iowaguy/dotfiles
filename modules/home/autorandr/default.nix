{ config, pkgs, ... }:

let
  fingerprintX1_2021 = {
    "eDP-1" =
      "00ffffffffffff000e6f031400000000001e0104b51e1378039d6baa5641af230e50580000000101010101010101010101010101010180e800a0f0605090302036002ebd10000018000000fd00303c95953c010a202020202020000000fe0043534f542054330a2020202020000000fe004d4e453030375a41312d320a20014f02030f00e3058000e60605016a6a24000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009a";
  };
  fingerprintLGHome = {
    "HDMI-1" =
      "00ffffffffffff001e6d085b162c0000091c0103803c2278ea3035a7554ea3260f50542108007140818081c0a9c0d1c081000101010108e80030f2705a80b0588a0058542100001e04740030f2705a80b0588a0058542100001a000000fd00383d1e873c000a202020202020000000fc004c4720556c7472612048440a2001c6020330714d902220050403020161605d5e5f230907076d030c001000b83c20006001020367d85dc401788003e30f0006023a801871382d40582c450058542100001a565e00a0a0a029503020350058542100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000aa";
  };
  fingerprintLGLab1 = {
    "DP-3" =
      "00ffffffffffff001e6d085b24020700091c0103803c2278ea3035a7554ea3260f50542108007140818081c0a9c0d1c081000101010108e80030f2705a80b0588a0058542100001e04740030f2705a80b0588a0058542100001a000000fd00383d1e873c000a202020202020000000fc004c4720556c7472612048440a2001db020330714d902220050403020161605d5e5f230907076d030c001000b83c20006001020367d85dc401788003e30f0006023a801871382d40582c450058542100001a565e00a0a0a029503020350058542100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000aa";
  };
  fingerprintDellVert = {
    "DP-3-1" =
      "00ffffffffffff0010acdca054463030211b010380351e78ea0565a756529c270f5054a54b00714f8180a9c0d1c00101010101010101023a801871382d40582c45000f282100001e000000ff00464d584e52373846303046540a000000fc0044454c4c205032343137480a20000000fd00384c1e5311000a2020202020200176020317b14c9005040302071601141f121365030c001000023a801871382d40582c45000f282100001e011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000000000000000000000000000000000000000000003d";
  };
  fingerprintDellVertHDMI = {
    "HDMI-1" =
      "00ffffffffffff0010acdca054463030211b010380351e78ea0565a756529c270f5054a54b00714f8180a9c0d1c00101010101010101023a801871382d40582c45000f282100001e000000ff00464d584e52373846303046540a000000fc0044454c4c205032343137480a20000000fd00384c1e5311000a2020202020200176020317b14c9005040302071601141f121365030c001000023a801871382d40582c45000f282100001e011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000000000000000000000000000000000000000000003d";
  };
  x1Config = {
    eDP-1 = {
      enable = true;
      primary = true;
      mode = "3840x2400";
      position = "0x0";
      crtc = 0;
    };
  };
  lgHomeConfig = {
    HDMI-1 = {
      primary = true;
      enable = true;
      mode = "3840x2160";
      crtc = 1;
      position = "3840x0";
      rate = "60.00";
    };
  };
  lgLabConfig = {
    DP-3 = {
      primary = true;
      enable = true;
      mode = "3840x2160";
      crtc = 1;
      position = "3840x0";
      rate = "60.00";
    };
  };
  dellLab2MonitorsConfig = {
    HDMI-1 = {
      primary = false;
      enable = true;
      mode = "1920x1080";
      crtc = 2;
      position = "7680x0";
      rotate = "left";
      rate = "60.00";
    };
  };
in
{
  programs.autorandr = {
    enable = true;
    hooks.postswitch = {
      "change-background" = "${pkgs.feh}/bin/feh --bg-fill ~/.background-image";
      "restart-xmonad" = "${pkgs.haskellPackages.xmonad}/bin/xmonad --restart";
    };
    profiles = {
      "unpluggedX1_2021" = {
        fingerprint = fingerprintX1_2021;
        config = x1Config;
      };
      # configure manually with: `xrandr --output eDP-1 --auto --output HDMI-1 --right-of eDP-1`
      # configure manually with: `xrandr --output eDP-1 --auto --output DP-3 --right-of eDP-1`
      "lab" = {
        fingerprint = fingerprintX1_2021 // fingerprintLGLab1;
        config = x1Config // lgLabConfig;
      };
      "lab2Monitors" = {
        # fingerprint = fingerprintX1_2021 // fingerprintLGLab2;
        fingerprint = fingerprintX1_2021 // fingerprintLGLab1 // fingerprintDellVertHDMI;
        # fingerprint = fingerprintX1_2021 // fingerprintLGLab2Monitors // fingerprintDellVert;
        config = x1Config // lgLabConfig // dellLab2MonitorsConfig;
      };
      "home" = {
        fingerprint = fingerprintX1_2021 // fingerprintLGHome;
        config = x1Config // lgHomeConfig;
      };
    };
  };
}
