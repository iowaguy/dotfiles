{ pkgs, pkgsUnstable, ... }:

{
  home.packages = with pkgsUnstable.python311Packages; [
    pandas
    numpy
    pymongo
    seaborn
  ];
}
