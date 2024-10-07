{ flakePkgs, ... }:
{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  cfg = config.services.giveandtake;
in
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];
  options.services.giveandtake = {
    enable = mkEnableOption "Enable Give'n'take service";
    host = mkOption { type = types.str; };
    postgres = {
      # USEME
      port = mkOption {
        type = types.int;
        default = "5432";
      };
    };
    docs = { };
    frontend = { };
    backend = {
      # USEME
      port = mkOption {
        type = types.int;
        default = "8080";
      };
    };
  };

  config = mkIf cfg.enable {
    services.nginx = {
      enable = true;

      virtualHosts = {
        "${cfg.host}" = {
          locations."/" = {
            root = "${flakePkgs.giveandtake-frontend}";
            extraConfig = ''
              # don't interpret url paths as file paths
              try_files $uri /index.html;
              proxy_http_version 1.1;
              proxy_set_header Connection "upgrade";
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header X-Forwarded-For $remote_addr;
              proxy_set_header X-Forwarded-Proto $scheme;
            '';
          };
          locations."/api" = {
            proxyPass = "http://localhost:8080";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header Connection "upgrade";
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header X-Forwarded-For $remote_addr;
              proxy_set_header X-Forwarded-Proto $scheme;
            '';
          };
          locations."/docs" = {
            root = "${flakePkgs.giveandtake-docs}";
            extraConfig = ''
              # don't interpret url paths as file paths
              try_files $uri /index.html;
              proxy_http_version 1.1;
              proxy_set_header Connection "upgrade";
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header X-Forwarded-For $remote_addr;
              proxy_set_header X-Forwarded-Proto $scheme;
            '';
          };
        };
      };
    };

    services.postgresql = {
      enable = true;
      ensureDatabases = [ "giveandtake" ];
      authentication = pkgs.lib.mkOverride 10 ''
        #type database  DBuser  auth-method
        local all       all     trust
      '';
      package = pkgs.postgresql_15;
    };
  };
}
