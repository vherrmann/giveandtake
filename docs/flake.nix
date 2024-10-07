{
  description = "giveandtake docs";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          packages = with pkgs; [
            watchexec
            (python3.withPackages (
              ps: with ps; [
                mkdocs
                mkdocs-material
                mkdocs-material-extensions
              ]
            ))
          ];
        };
      }
    );
}
# pkill dufs && nix run nixos#dufs -- -p 8000 site/ --path-prefix="/docs" --render-index & && watchexec -e yaml -e md mkdocs build
