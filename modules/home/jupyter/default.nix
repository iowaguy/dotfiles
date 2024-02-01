{ pkgs, pkgsUnstable, ... }:

{
  home.packages = with pkgsUnstable; [
    # python311
    python311Packages.pandas
    python311Packages.numpy
    python311Packages.pymongo
    python311Packages.seaborn
  ];
}
