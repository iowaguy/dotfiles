#!/bin/sh -e

nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager
nix-channel --update

nix-shell '<home-manager>' -A install

ln -s $PWD/nix/home.nix $HOME/.config/nixpkgs/home.nix

home-manager switch

