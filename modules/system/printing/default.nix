{
  services.printing = {
    enable = true;
    extraConf = builtins.readFile ./cups.conf;
  };
}
