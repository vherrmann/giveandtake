{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
    backend = {
      # FIXME:
      url = "github:vherrmann/giveandtake?dir=server";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      gitignore,
      backend,
      ...
    }@inputs:
    (flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
      ]
      (
        system:
        let
          overlays = [
            (final: prev: {
              giveandtake-frontend = final.callPackage (import ./nix/frontend.nix inputs) { };
              giveandtake-docs = final.callPackage (import ./nix/docs.nix inputs) { };
              giveandtake-backend = backend.defaultPackage."${system}";
            })
          ];
          pkgs = import nixpkgs { inherit overlays system; };
        in
        {
          packages = {
            frontend = pkgs.giveandtake-frontend;
            docs = pkgs.giveandtake-docs;
            backend = pkgs.giveandtake-backend;
          };
          nixosModules.default = import ./nix/nixosModule.nix {
            inherit inputs;
            flakePkgs = pkgs; # FIXME: ideally a consument of the nixos module would just use his own pkgs (or maybe not?)
          };
        }
      )
    )
    // {
      # FIXME: hack to make nixos-containers work
      nixosConfigurations.container = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.x86_64-linux.default
          ({
            services.giveandtake = {
              enable = true;
              host = "localhost:8090";
            };
            system.stateVersion = "24.05";
          })
        ];
      };
    };
}
