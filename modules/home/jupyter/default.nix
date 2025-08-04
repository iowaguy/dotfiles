{ pkgs, pkgsUnstable, ... }:

{
  home.packages = with pkgsUnstable.python312Packages; [
    pandas
    numpy
    pymongo
    seaborn
  ];
}
