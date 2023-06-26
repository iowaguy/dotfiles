{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ../../system/configuration.nix
    ./hardware.nix
    ./machine.nix
  ];
}
