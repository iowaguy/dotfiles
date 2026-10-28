{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    pass-git-helper # for letting git get passwords from pass
    diff-so-fancy # git diff with colors
    python314Packages.git-filter-repo
  ];

  xdg.configFile."pass-git-helper/git-pass-mapping.ini".text = ''
    [achtung-gitlab.ccs.neu.edu*]
    target=achtung-gitlab
  '';
  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ".direnv" ".envrc" ];
    lfs.enable = true;
    settings = {
      user.email = "ben@weintraub.xyz";
      user.name = "Ben Weintraub";
      protocol.file.allow = "always";
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
        excludesfiles = "${config.home.homeDirectory}/.gitignore";
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
          helper = "${pkgs.pass-git-helper}/bin/pass-git-helper";
        };
      };
      rebase.autosquash = true;
      init.defaultBranch = "main";
    };
  };
}
