{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    # ../../configuration.nix
    ./hardware.nix
    ./machine.nix
  ];
}
