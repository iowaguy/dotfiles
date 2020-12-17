#!/bin/sh

# This is for auto-selfcontrol: https://github.com/andreasgrill/auto-selfcontrol
brew tap andreasgrill/utils
brew tap railwaycat/emacsmacport

brew install jpeg \
             autoconf \
             automake \
             coreutils \
             pandoc \
             curl \
             libevent \
             postgresql \
             gdb \
             protobuf \
             python \
             libpng \
             ghostscript \
             qt \
             glib \
             libtiff \
             libtool \
             rlwrap \
             ruby \
             gnu-getopt \
             gnu-tar \
             libyaml \
             sqlite \
             gnupg \
             gnuplot \
             llvm \
             stow \
             gnutls \
             lzo \
             the_silver_searcher \
             tree \
             maven \
             mu \
             ncurses \
             wget \
             htop \
             httpie \
             node \
             xapian \
             icu4c \
             xmlto \
             npth \
             xz \
             zsh-syntax-highlighting \
             zsh \
             ispell \
             iterm \
             isync \
             auto-selfcontrol \
             msmtp \
             sdcv \
             pipx \
             pssh \
             graphviz

# This needs its own line, because it includes extra switches
brew install emacs-mac --with-modules --with-xml2 --with-imagemagick

brew cask install selfcontrol
