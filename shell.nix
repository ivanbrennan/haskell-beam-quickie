let
  nixpkgs = import <nixpkgs>;

  pkgs = nixpkgs { };

  hPkgs = ps: with ps; [
    beam-core
    beam-migrate
    beam-sqlite
    pretty-simple
    sqlite-simple
  ];

in with pkgs; mkShell {
  buildInputs = [
    gnumake
    (haskellPackages.ghcWithPackages hPkgs)
    haskellPackages.ghcid
    sqlite
  ];
}
