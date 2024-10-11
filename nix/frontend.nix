{ inputs, ... }:
{
  pkgs,
  lib,
  stdenv,
  ...
}:
let
  inherit (inputs.gitignore.lib) gitignoreSource;
in
(pkgs.buildNpmPackage {
  name = "giveandtake";
  src = gitignoreSource ../client/.;
  npmDepsHash = "sha256-D/9YjHd8e2Ou4VJCEfZMVg70UZs4JgaG2gtPA0pzBJc";
}).overrideAttrs
  (oldAttrs: {
    installPhase = ''
      mv build $out
    '';
  })
