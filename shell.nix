{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [ hakyll ]))
    # cabal2nix
  ];
}
