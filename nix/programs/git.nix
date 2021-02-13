{
  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "benweintraub34@gmail.com";
    userName = "Ben Weintraub";
    extraConfig.pull.rebase = "true";
    extraConfig.github.user = "iowaguy";
  };
}
