#!/bin/sh

# Install the necessary MacOS tools, like Homebrew and Xcode tools
./install_mac_packages.sh

# Install all the necessary brew packages, especially `stow`.
./install_brew_packages.sh


# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
upgrade_oh_my_zsh

stow --dotfiles zsh
stow --dotfiles emacs
stow --dotfiles email
stow --dotfiles firefox
stow --dotfiles git

# Many of the tools in this repo are written in Ruby, and some depend on
# external libraries. This installs those.
./install_ruby_gems.sh

# Similarly, we use a couple of Python packages.
./install_python_packages.sh

# Finally, the Node packages. Dang, computers are complicated.
./install_node_packages.sh
