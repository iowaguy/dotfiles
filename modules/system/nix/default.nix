{ pkgs, ...}:
{
  # Nix daemon config
  nix = {
    # Automate garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # TODO remove when flakes are included by default
    package = pkgs.nixVersions.stable;

    extraOptions = ''
      # TODO remove when flakes are included by default
      experimental-features = nix-command flakes

      # Avoid unwanted garbage collection when using nix-direnv
      keep-outputs     = true
      keep-derivations = true
    '';

    settings = {
      # Required by Cachix to be used as non-root user
      trusted-users = [ "root" "ben" ];

      # Automate `nix-store --optimise`
      auto-optimise-store = true;
    };
  };
}
