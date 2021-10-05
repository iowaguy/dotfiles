#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

import json


# read nix/sources.json
# for each key
#   run: niv update <key> -b <branch>
