{ config, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ben = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHA2+s16j8CHT54sw3eenPv48zg1gHzSabsRhkEt87Ss ben@weintraub.xyz"
    ];
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable 'sudo' for the user.
      "networkmanager" # Allow user to change network settings
      "docker"
      "libvirtd"
      "power"
      "syncthing"
    ];
    shell = pkgs.fish;
  };
}
