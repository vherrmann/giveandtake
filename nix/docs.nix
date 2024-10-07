{ gitignore, ... }:
{
  pkgs,
  lib,
  stdenv,
  ...
}:

let
  inherit (gitignore.lib) gitignoreSource;
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
    ${myMkdocs}/bin/mkdocs build
  '';

  installPhase = ''
    mv site $out
  '';
}
