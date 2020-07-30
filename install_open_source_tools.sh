#!/bin/sh

# Start in the directory where I put all these projects and downloads
pushd ~/.opt/

# Download gdb-dashboard
git clone https://github.com/cyrus-and/gdb-dashboard.git
ln -s .gdbinit ~/.gdbinit

# Download sensible defaults (for Emacs)
git clone https://github.com/hrs/sensible-defaults.el.git

# Install my auto-selfcontrol configs
ln -s auto-selfcontrol/config.json /usr/local/etc/auto-selfcontrol/

# Turn on auto-selfcontrol
auto-selfcontrol activate

popd
