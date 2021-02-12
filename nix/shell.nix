with import <nixpkgs> { };

let
  pythonPackages = ps:
    with ps; [
      # Linting
      black
      mypy
      pylint

      gi
    ];
in pkgs.mkShell rec {
  buildInputs = with pkgs; [
    stdenv

    # Dev environment
    vagrant

    # Python development
    (python38.withPackages pythonPackages)
  ];
}
