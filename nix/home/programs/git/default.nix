{ pkgs, ... }:

{
  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
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
        gpgsign = false;
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
      rebase.autosquash = true;
    };
  };

}
