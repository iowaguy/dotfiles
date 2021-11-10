with import <nixpkgs> {};
let
  pythonPackages = ps:
    with ps; [
      # This execute some shell code to initialize a venv in $venvDir before
      # dropping into the shell
      venvShellHook

      # Linting
      black
      mypy
      pylint
    ];
in mkShell rec {
  venvDir = "./.venv";
  buildInputs = [
    python

    # Linting + development
    nodePackages.pyright
    hello
    bashInteractive

    # Python development
    (python38.withPackages pythonPackages)
  ];

  # Run this command, only after creating the virtual environment
  postVenvCreation = ''
    unset SOURCE_DATE_EPOCH
    for requirements_file in requirements*.txt; do
      pip install -r $requirements_file
    done
  '';

  postShellHook = ''
    # allow pip to install wheels
    unset SOURCE_DATE_EPOCH
  '';

}
