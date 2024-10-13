{ inputs, system, ... }:
{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  cfg = config.services.giveandtake;
  gatPkgs = {
    frontend = pkgs.callPackage (import ./frontend.nix { inherit inputs; }) { };
    docs = pkgs.callPackage (import ./docs.nix {
      inherit inputs;
      baseUrl = cfg.docsBaseUrl;
    }) { };
    backend = inputs.backend.defaultPackage."${system}";
  };
in
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];

  # FIXME: add doc to options
  options.services.giveandtake = {
    enable = mkEnableOption "Enable Give'n'take service";

    baseUrl = mkOption {
      type = types.str;
      default = cfg.host;
    };
    postgres = {
      # USEME
      port = mkOption {
        type = types.int;
        default = "5432";
      };
    };
    user = mkOption {
      type = types.str;
      default = "giveandtake";
    };
    group = mkOption {
      type = types.str;
      default = cfg.user;
    };
    docs = { };
    frontend = { };
    docsBaseUrl = mkOption { type = types.str; };
    backend = {
      # USEME
      port = mkOption {
        type = types.int;
        default = 8080;
      };
      # FIXME: ensure mediaDir
      mediaDir = mkOption {
        type = types.str;
        default = "/var/lib/giveandtake/media";
      };
      serviceName = mkOption {
        type = types.str;
        default = "Give'n'take";
      };
      dbConfig = {
        connections = mkOption { type = types.int; };
        connectionString = mkOption { type = types.str; };
      };
      emailConfig = {
        smtpHost = mkOption { type = types.str; };
        smtpPort = mkOption {
          type = types.int;
          default = 465;
        };
        smtpUser = mkOption { type = types.str; };
        smtpFrom = mkOption {
          type = types.str;
          default = cfg.backend.emailConfig.smtpUser;
        };
        smtpMethod = mkOption {
          type = types.enum [
            "StartTLS"
            "SSL"
            "Plain"
          ];
          default = "SSL";
          description = "You should always use SSL unless this service runs on the same host as the smtp server";
        };
        smtpPassFile = mkOption { type = types.str; };
      };
      timeout = mkOption { type = types.int; };
    };
  };

  config =
    let
      configFile = pkgs.writeTextFile {
        name = "config.yaml";
        text = builtins.toJSON {
          host = "localhost";
          inherit (cfg.backend)
            port
            dbConfig
            mediaDir
            serviceName
            emailConfig
            timeout
            ;
          inherit (cfg) docsBaseUrl baseUrl;
        };
      };
    in
    mkIf cfg.enable {
      systemd.tmpfiles.rules = [ "d ${cfg.backend.mediaDir} 0750 ${cfg.user} ${cfg.group} - -" ];

      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedOptimisation = true;

        virtualHosts = {
          # nginx apparently can't have more than one try_files, we therefore use static-web-server to serve the frontend
          "${cfg.baseUrl}" = {
            locations."/" = {
              root = "${gatPkgs.frontend}";
              extraConfig = ''
                # don't interpret url paths as file paths
                try_files $uri /index.html;
              '';
              proxyWebsockets = true; # FIXME: remove this?
            };
            locations."/api" = {
              proxyPass = "http://localhost:${toString cfg.backend.port}";
              extraConfig = ''
                # FIXME: add option for this (and the option in the server)
                client_max_body_size 500M;
              '';
              proxyWebsockets = true; # FIXME: remove this?
            };
          };
          "${cfg.docsBaseUrl}" = {
            locations."/" = {
              root = "${gatPkgs.docs}";
              proxyWebsockets = true; # FIXME: remove this?
            };
          };
        };

      };

      users.groups = {
        "${cfg.group}" = { };
      };

      users.users."${cfg.user}" = {
        description = "Give'n'take";
        group = "${cfg.group}";
        isSystemUser = true;
      };

      environment.systemPackages = [ gatPkgs.backend ];

      systemd.services.giveandtake = {
        enable = true;
        description = "Give'n'take";
        serviceConfig = {
          User = "${cfg.user}";
          Group = "${cfg.group}";
        };
        wantedBy = [ "multi-user.target" ];
        after = [ "postgresql.service" ];
        requires = [ "postgresql.service" ];
        script = ''
          ${gatPkgs.backend}/bin/giveandtake ${configFile}
        '';
      };

      # https://wiki.nixos.org/w/index.php?title=PostgreSQL
      services.postgresql = {
        enable = true;
        ensureDatabases = [ "giveandtake" ];
        ensureUsers = [
          {
            name = "giveandtake";
            ensureDBOwnership = true;
          }
        ];
        authentication = pkgs.lib.mkOverride 10 ''
          #type database  DBuser  auth-method
          local sameuser  all     peer        map=superuser_map
        '';
        identMap = ''
          # ArbitraryMapName systemUser DBUser
          superuser_map      root      postgres
          superuser_map      postgres  postgres
          # Let other names login as themselves
          superuser_map      /^(.*)$   \1
        '';
        package = pkgs.postgresql_15;
      };
    };
}
