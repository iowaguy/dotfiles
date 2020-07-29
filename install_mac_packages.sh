#!/bin/sh

# Install the CLI tools for Xcode, must be done before installing Homebrew
xcode-select —-install

# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
