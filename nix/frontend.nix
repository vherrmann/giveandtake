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
  npmDepsHash = "sha256-WIXDl04xqNXrzlqeNsReJ82mFpkGUTxAtjRo3dIo3OM=";
}).overrideAttrs
  (oldAttrs: {
    installPhase = ''
      mv build $out
    '';
  })
