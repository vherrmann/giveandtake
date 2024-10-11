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
      (system: {
        nixosModules.default = import ./nix/nixosModule.nix { inherit inputs system; };
      })
    )
    // {
      # FIXME: hack to make nixos-containers work
      nixosConfigurations.container = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.x86_64-linux.default
          ({
            # FIXME: extend doc
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
