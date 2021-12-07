#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 niv

import json
import subprocess
import os

sources_path = f"{os.getenv('HOME')}/workspace/dotfiles/nix/sources.json"

if __name__ == "__main__":
    with open(sources_path, 'r') as f:
        sources_dict = json.load(f)

    for key, val in sources_dict.items():
        if 'branch' in val:
            subprocess.run(["niv", "update", key, "-b", val['branch']])
