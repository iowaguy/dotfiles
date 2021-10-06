#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 niv

import json
import subprocess

sources_path = "nix/sources.json"

if __name__ == "__main__":
    with open(sources_path, 'r') as f:
        sources_dict = json.load(f)

    for key, val in sources_dict.items():
        subprocess.run(["niv", "update", key, "-b", val['branch']])
