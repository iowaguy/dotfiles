{pkgs, ...}:
{
  environment.variables = {
    XSECURELOCK_SAVER = "saver_blank";
    XSECURELOCK_AUTH = "auth_pam_x11";
    XSECURELOCK_SHOW_DATETIME = "1";
  };

    # Lock screen before sleeping
  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.xsecurelock}/bin/xsecurelock";
  };

  services.logind.lidSwitchDocked = "suspend";
}
