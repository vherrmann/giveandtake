{
  description = "giveandtake server";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    servant-openapi3.url = "github:vherrmann/servant-openapi3?ref=required-request-bodies"; # "github:vherrmann/servant-openapi3";
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
          developInputs =
            (with compiler; [
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
          # fixme: remove development dependencies
          nativeBuildInputs = with compiler; [ cabal-install ];
          runtimeInputs = with pkgs; [
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
                };
              withHoogle = true;
              # FIXME: haddock build breaks on server
              cabal2nixOptions = "--no-haddock";
              modifier =
                drv:
                pkgs.haskell.lib.addBuildTools drv (
                  (if returnShellEnv then developInputs ++ runtimeInputs else [ ]) ++ nativeBuildInputs
                );
            };
          wrapBinWithDeps =
            binary: dependencies: pkg:
            pkgs.symlinkJoin {
              name = pkg.name;
              paths = [ pkg ];
              buildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/${binary} $wrapperfile --prefix PATH : ${pkgs.lib.makeBinPath dependencies}
              '';
            };
        in
        {
          # Used by `nix build` & `nix run` (prod exe)
          defaultPackage = wrapBinWithDeps "giveandtake" runtimeInputs (projectHs false);

          # `nix develop`
          devShell = projectHs true;
        }
      );
}
