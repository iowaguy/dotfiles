#!/bin/sh

# Install the necessary MacOS tools, like Homebrew and Xcode tools
./install_mac_packages.sh

# Install all the necessary brew packages, especially `stow`.
./install_brew_packages.sh


# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
upgrade_oh_my_zsh

stow -t ~ --dotfiles zsh
stow -t ~ --dotfiles emacs
stow -t ~ --dotfiles email
stow -t ~ --dotfiles firefox
stow -t ~ --dotfiles git
stow -t ~ --dotfiles open-source-tools

# Many of the tools in this repo are written in Ruby, and some depend on
# external libraries. This installs those.
./install_ruby_gems.sh

# Similarly, we use a couple of Python packages.
./install_python_packages.sh

# The Node packages. Dang, computers are complicated.
./install_node_packages.sh

# Some useful repositories and downloads
./install_open_source_tools.sh
