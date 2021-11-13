with import <nixpkgs> {};
let
  ## NOTE This is how to add package that isn't ported to nix yet
  # web3 = python38.pkgs.buildPythonPackage rec {
  #   pname = "web3";
  #   version = "5.24.0";

  #   src = pkgs.fetchurl {
  #     # inherit pname version;
  #     sha256 = "6535618b07a8d3d7496374a3b7714cfade4f94e6dc085a518718d4c6f776ac3f";
  #     url = "https://files.pythonhosted.org/packages/ea/4c/3d0d300df8f0c3937bb00a069d4a7b9b43603fdbcd411829449c5b4d6383/web3-5.24.0.tar.gz";
  #   };

  #   doCheck = false;

  #   meta = {
  #     homepage = "https://github.com/ethereum/web3.py";
  #     description = "A Python library for interacting with Ethereum, inspired by web3.js.";
  #   };
  #   propagatedBuildInputs = [
  #     hexbytes
  #     pkgs.python38Packages.protobuf
  #     pkgs.python38Packages.aiohttp
  #   ];
  # };

  pythonPackages = ps:
    with ps; [
      # # This execute some shell code to initialize a venv in $venvDir before
      # # dropping into the shell
      # venvShellHook

      # Linting
      black
      mypy
      pylint
    ];
in mkShell rec {
  # venvDir = "./.venv";
  buildInputs = [
    python

    # Linting + development
    nodePackages.pyright
    hello
    bashInteractive

    # Python development
    (python38.withPackages pythonPackages)
  ];

  ## NOTE only need this for handling environment with requirements.txt
  # # Run this command, only after creating the virtual environment
  # postVenvCreation = ''
  #   unset SOURCE_DATE_EPOCH
  #   for requirements_file in requirements*.txt; do
  #     pip install -r $requirements_file
  #   done
  # '';

  # postShellHook = ''
  #   # allow pip to install wheels
  #   unset SOURCE_DATE_EPOCH
  # '';

}
