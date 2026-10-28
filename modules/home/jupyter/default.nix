{ pkgs, pkgsUnstable, ... }:

{
  home.packages = with pkgsUnstable.python314Packages; [
    pandas
    numpy
    pymongo
    seaborn
  ];
}
