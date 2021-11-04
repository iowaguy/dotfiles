with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    hello
    bashInteractive
  ];
}
