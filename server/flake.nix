{
  description = "giveandtake server";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    servant-openapi3.url = "/home/vherrmann/repos/servant-openapi3?ref=required-request-bodies"; # "github:vherrmann/servant-openapi3";
    servant-openapi3.flake = false;
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      name = "giveandtake";
    in
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
      ]
      (
        system:
        let
          overlays = [ ];
          pkgs = import nixpkgs { inherit system overlays; };
          compilerVersion = "ghc96";
          compiler = pkgs.haskell.packages."${compilerVersion}";
          # fixme: remove development dependencies
          nativeBuildInputsHs =
            (with compiler; [
              cabal-install
              hlint
              haskell-language-server
              fourmolu
            ])
            ++ (with pkgs; [
              nixpkgs-fmt
              treefmt
              sqlitebrowser
              watchexec
              openapi-generator-cli # for giveandtake-generate-typescript
              devenv
            ]);
          buildInputsHs = with pkgs; [
            ffmpeg
            imagemagick
          ];
          projectHs =
            returnShellEnv:
            compiler.developPackage {
              inherit returnShellEnv;
              name = name;
              root = ./.;
              overrides =
                self: super: with pkgs.haskell.lib; {
                  "servant-openapi3" = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "servant-openapi3"
                    inputs.servant-openapi3
                    { }
                  ) (old: { });
                  "servant-auth-server" = (
                    pkgs.haskell.lib.overrideCabal super.servant-auth-server (old: {
                      broken = false;
                      doCheck = false;
                    })
                  );
                  # "servant-aeson-generics-typescript" = (
                  #   pkgs.haskell.lib.overrideCabal super.aeson-generics-typescript (old: {
                  #     jailbreak = true;
                  #     broken = false;
                  #     doCheck = false;
                  #   })
                  # );
                };
              withHoogle = true;
              modifier =
                drv:
                pkgs.haskell.lib.addExtraLibraries (pkgs.haskell.lib.addBuildTools drv nativeBuildInputsHs) buildInputsHs;
            };
        in
        {
          # Used by `nix build` & `nix run` (prod exe)
          defaultPackage = projectHs false;

          # `nix develop`
          devShell = projectHs true;
        }
      );
}
