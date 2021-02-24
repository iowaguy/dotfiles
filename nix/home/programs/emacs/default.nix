{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    extraPackages = epkgs: [
      epkgs.use-package
      pkgs.ispell
    ];
    enable = true;
  };

  home.links.".emacs.d" = "./workspace/dotfiles/emacs/dot-emacs.d";
}
