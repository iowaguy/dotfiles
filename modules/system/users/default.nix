{ pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    ben = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHA2+s16j8CHT54sw3eenPv48zg1gHzSabsRhkEt87Ss ben@weintraub.xyz"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGVxtjrPG1RJTXrI+7ftjQLfqgwL2GXDfAGyaVA1Qaaa ben@x1-2021"
      ];
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable 'sudo' for the user.
        "networkmanager" # Allow user to change network settings
        "docker"
        "qemu-libvirtd"
        "libvirtd"
        "power"
        "syncthing"
      ];
      shell = pkgs.zsh;
    };

    root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGVxtjrPG1RJTXrI+7ftjQLfqgwL2GXDfAGyaVA1Qaaa ben@x1-2021"
    ];
  };
}
