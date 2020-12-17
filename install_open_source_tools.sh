#!/bin/sh

# Start in the directory where I put all these projects and downloads
pushd ~/.opt/

# Download gdb-dashboard
git clone https://github.com/cyrus-and/gdb-dashboard.git
ln -s .gdbinit ~/.gdbinit

# Download sensible defaults (for Emacs)
git clone https://github.com/hrs/sensible-defaults.el.git

# Install my auto-selfcontrol configs
rm -f /usr/local/etc/auto-selfcontrol/config.json
mkdir /usr/local/etc/auto-selfcontrol/
ln -s ~/.opt/auto-selfcontrol/config.json /usr/local/etc/auto-selfcontrol/

# Turn on auto-selfcontrol
auto-selfcontrol activate

popd
