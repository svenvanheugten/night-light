{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-check-assertions = {
      url = "git+https://codeberg.org/svenvanheugten/git-check-assertions.git?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-check-assertions,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.callPackage ./default.nix { };
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.dotnet-sdk_10
            git-check-assertions.packages.${system}.default
          ];
        };
      }
    )
    // {
      nixosModules = {
        night-light = import ./module.nix self;
      };
    };
}
