{ pkgs, ... }:

{
  home.file.".local/share/applications/skype.desktop".source = "${builtins.getEnv "HOME"}/workspace/dotfiles/nix/programs/skype/skype.desktop";
}
