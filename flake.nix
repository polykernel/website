{
  description = "A very basic flake";

  nixConfig = {
    bash-prompt = "\\n\\[\\033[1;33m\\][\\[\\e]0;nix-develop: \\w\\a\\]nix-develop:\\w]λ \\[\\033[0m\\]";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;
      supportedSystems = [ "x86_64-linux" "i686-linux" ];
      forAllSystems = lib.genAttrs supportedSystems;
    in {
      devShell = forAllSystems (system: import ./shell.nix { pkgs = nixpkgs.legacyPackages.${system}; });
    };
}
