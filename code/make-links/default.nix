{ config, lib, pkgs, ... }:

with lib;

let

  dag = config.lib.dag;
  cfg = config.home.links;
  homeDirectory = config.home.homeDirectory;

in

{

  options = {
    home.links = mkOption {
      type = with types; attrsOf str;
      default = {};
      example = { "link/target" = "link/source"; };
      description = "Extra symlinks to make (to non-managed files).";
    };
  };

  config = let

    generateLinkSh = targetName: sourceName: (
      let
        source = "${homeDirectory}/${sourceName}";
        target = "${homeDirectory}/${targetName}";
      in ''
        if [ -h "${target}" ]; then
          CURR_LINK_SOURCE=$(readlink -f "${target}")
          if [ "$CURR_LINK_SOURCE" != "${source}" ]; then
              warnEcho "${target} points to wrong place; updating."
              $DRY_RUN_CMD rm "${target}"
              $DRY_RUN_CMD ln -snT "${source}" "${target}"
          fi
        elif [ -e "${target}" ]; then
          errorEcho "${target} exists but is not a link."
          exit 1
        else
          $DRY_RUN_CMD ln -snT "${source}" "${target}"
        fi
      '');

  in

    {
      home.activation.makeLinks =
        dag.entryAfter [ "writeBoundary" ] ''
          ${concatStringsSep "\n" (mapAttrsToList generateLinkSh cfg)}
        '';
    };

}
