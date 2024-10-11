{ inputs, baseUrl, ... }:
{
  pkgs,
  lib,
  stdenv,
  ...
}:

let
  inherit (inputs.gitignore.lib) gitignoreSource;
  myMkdocs = pkgs.python3.withPackages (
    ps: with ps; [
      mkdocs
      mkdocs-material
      mkdocs-material-extensions
    ]
  );
in
stdenv.mkDerivation rec {
  name = "giveandtakedocs";
  src = gitignoreSource ../docs;

  buildPhase = ''
    ${pkgs.yq-go}/bin/yq eval '.site_url = "https://${baseUrl}/docs"' -i mkdocs.yml
    ${myMkdocs}/bin/mkdocs build
  '';

  installPhase = ''
    mv site $out
  '';
}
