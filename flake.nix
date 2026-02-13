{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.callPackage ./default.nix { };
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.dotnet-sdk_10 ];
        };
      }
    )
    // {
      nixosModules = {
        night-light = import ./module.nix;
      };
    };
}
