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
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ ];
          };
        in
        {
          nixosModules.default = import ./nix/nixosModule.nix { inherit inputs system; };
          # FIXME: exists to test if the build succeeds
          packages = {
            backend = inputs.backend.defaultPackage."${system}";
            frontend = pkgs.callPackage (import ./nix/frontend.nix {
              inherit inputs;
              docsBaseUrl = "";
            }) { };
            docs = pkgs.callPackage (import ./nix/docs.nix {
              inherit inputs;
              docsBaseUrl = "";
            }) { };
          };
        }
      )
    )
    // {
      # FIXME: hack to make nixos-containers work
      # Test with `nixos-rebuild build --flake .#container`
      nixosConfigurations.container = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.x86_64-linux.default
          ({
            # FIXME: extend doc
            services.giveandtake = {
              enable = true;
              user = "giveandtake";
              baseUrl = "giveandtake.com";
              docsBaseUrl = "docs.giveandtake.com";
              backend = {
                dbConfig = {
                  connections = 10;
                  connectionString = "";
                };
                timeout = 600;
                emailConfig = {
                  smtpHost = "gmail.com";
                  smtpUser = "giveandtake@gmail.com";
                  smtpPassFile = "";
                };
              };
            };
            system.stateVersion = "24.05";
          })
        ];
      };
    };
}
