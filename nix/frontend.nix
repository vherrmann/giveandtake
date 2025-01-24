{ inputs, docsBaseUrl, ... }:
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
  npmDepsHash = "sha256-u7D8NaSAPdEXGSB+Vg4x2mqnhQP80kibWGCBbJ3J/N0="; # lib.fakeHash
}).overrideAttrs
  (oldAttrs: {
    postConfigure = ''
      ${pkgs.yq-go}/bin/yq eval '.docsBaseUrl = "${docsBaseUrl}"' -i config.json
    '';
    installPhase = ''
      mv dist $out
    '';
  })
