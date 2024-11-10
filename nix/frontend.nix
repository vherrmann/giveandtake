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
  npmDepsHash = "sha256-0ZX/L1VBoDQLd99IadBwSlDDVjk1ynv7LisyaOEguPY=";
}).overrideAttrs
  (oldAttrs: {
    postConfigure = ''
      ${pkgs.yq-go}/bin/yq eval '.docsBaseUrl = "${docsBaseUrl}"' -i config.json
    '';
    installPhase = ''
      mv dist $out
    '';
  })
