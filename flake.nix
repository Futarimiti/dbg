{
  description = "dbg macro reinvented in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = rec {
          dbg = import ./default.nix { inherit pkgs; };
          default = dbg;
        };
        apps = rec {
          dbg = flake-utils.lib.mkApp { drv = self.packages.${system}.dbg; };
          default = dbg;
        };
        devShells.default = import ./shell.nix { inherit pkgs; };
      }
    );
}
