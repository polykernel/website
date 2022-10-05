{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    haskellPackages.hakyll
    cabal2nix
  ];
}
