#!/bin/sh

# Install the CLI tools for Xcode, must be done before installing Homebrew
sudo xcode-select —-install

echo "If previous command fails, install xcode manually from: https://developer.apple.com/download/more/"

# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
