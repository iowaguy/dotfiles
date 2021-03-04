#! /usr/bin/env bash

# Shows the output of every command
set +x

# # Pin Nixpkgs to NixOS unstable on Feb 21st of 2021
# export PINNED_NIX_PKGS="https://github.com/NixOS/nixpkgs/archive/9816b99e71c.tar.gz"
# # Switch to the unstable channel
# sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos

# Nix configuration
sudo cp system/configuration.nix /etc/nixos/
# sudo cp -r system/fonts/ /etc/nixos/
# sudo cp -r system/machine/ /etc/nixos/
sudo cp -r system/wm/ /etc/nixos/
sudo cp -r system/services/ /etc/nixos/
sudo nixos-rebuild switch

ln -s $HOME/workspace/dotfiles/home/home.nix $HOME/.config/nixpkgs/home.nix
home-manager switch
