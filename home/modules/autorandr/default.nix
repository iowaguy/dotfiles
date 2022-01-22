{ config, pkgs, ... }:

let
  fingerprintMBP = {
    "eDP" =
      "00ffffffffffff0006102fa00000000004190104a5211578026fb1a7554c9e250c505400000001010101010101010101010101010101ef8340a0b0083470302036004bcf1000001a000000fc00436f6c6f72204c43440a20202000000010000000000000000000000000000000000010000000000000000000000000000000cf";
  };
  fingerprintX1_2021 = {
    "eDP-1" =
      "00ffffffffffff000e6f031400000000001e0104b51e1378039d6baa5641af230e50580000000101010101010101010101010101010180e800a0f0605090302036002ebd10000018000000fd00303c95953c010a202020202020000000fe0043534f542054330a2020202020000000fe004d4e453030375a41312d320a20014f02030f00e3058000e60605016a6a24000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009a";
  };
  fingerprintLGLab = {
    "HDMI-0" =
      "00ffffffffffff001e6d085b162c0000091c0103803c2278ea3035a7554ea3260f50542108007140818081c0a9c0d1c081000101010108e80030f2705a80b0588a0058542100001e04740030f2705a80b0588a0058542100001a000000fd00383d1e873c000a202020202020000000fc004c4720556c7472612048440a2001c6020330714d902220050403020161605d5e5f230907076d030c001000b83c20006001020367d85dc401788003e30f0006023a801871382d40582c450058542100001a565e00a0a0a029503020350058542100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000aa";
  };
in
{
  programs.autorandr.profiles = {
    "unpluggedMBP" = {
      fingerprint = fingerprintMBP;
      config = {
        "eDP" = {
          enable = true;
          mode = "1920x1080";
        };
      };
    };
    "unpluggedX1_2021" = {
      fingerprint = fingerprintX1_2021;
      config = {
        "eDP-1" = {
          enable = true;
          primary = true;
          mode = "3840x2400";
        };
      };
    };
    "home_X1_2021" = {
      fingerprint = fingerprintX1_2021 // fingerprintLGLab;
      config = {
        "eDP-1" = {
          enable = true;
          position = "0x0";
          primary = true;
          mode = "3840x2400";
          crtc = 0;
        };
        "HDMI-1" = {
          enable = true;
          position = "2840x0";
          mode = "3840x2160";
          crtc = 1;
        };
      };
    };
  };
}
