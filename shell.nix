{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [ hakyll pandoc ]))
    # cabal2nix
  ];
}
