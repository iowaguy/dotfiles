{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gitAndTools.pass-git-helper # for letting git get passwords from pass
    gitAndTools.diff-so-fancy # git diff with colors
  ];

  xdg.configFile."pass-git-helper/git-pass-mapping.ini".text = ''
    [achtung-gitlab.ccs.neu.edu*]
    target=achtung-gitlab
  '';
  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ".direnv" ".envrc" ];
    userEmail = "ben@weintraub.xyz";
    userName = "Ben Weintraub";
    extraConfig = {
      pull.rebase = "true";
      github.user = "iowaguy";
      branch.autosetuprebase = "always";
      color.ui = true;
      "color \"diff\"".meta = "bold cyan";
      "color \"grep\"" = {
        filename = "magenta";
        match = "bold red";
        linenumber = "bold blue";
      };
      commit = {
        gpgsign = true;
      };
      core = {
        excludesfiles = "${builtins.getEnv "HOME"}/.gitignore";
        pager = "diff-so-fancy | less --tabs=4 -RFX";
      };
      fetch.prune = true;
      grep.linenumber = true;
      merge.ff = "only";
      push = {
        default = "current";
        followTags = true;
      };
      credential = {
        "https://achtung-gitlab.ccs.neu.edu" = {
          username = "iowaguy";
          helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
        };
      };
      rebase.autosquash = true;
    };
  };
}
