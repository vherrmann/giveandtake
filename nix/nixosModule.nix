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

  # FIXME: add doc to options
  options.services.giveandtake = {
    enable = mkEnableOption "Enable Give'n'take service";
    listenAddress = mkOption { type = types.str; };
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
      baseUrl = mkOption {
        type = types.str;
        default = cfg.host;
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
        smtpPort = mkOption { type = types.int; };
        smtpUser = mkOption { type = types.str; };
        smtpFrom = mkOption {
          type = types.str;
          default = cfg.backend.emailConfig.smtpUser;
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
          port = cfg.backend.port;
          dbConfig = cfg.backend.dbConfig;
          mediaDir = cfg.backend.mediaDir;
          authority = cfg.backend.baseUrl;
          serviceName = cfg.backend.serviceName;
          timeout = cfg.backend.timeout;
          emailConfig = cfg.backend.emailConfig;
        };
      };
    in
    mkIf cfg.enable {
      systemd.tmpfiles.rules = [ "d ${cfg.backend.mediaDir} 0750 ${cfg.user} ${cfg.group} - -" ];

      services.nginx = {
        enable = true;

        virtualHosts = {
          "${cfg.listenAddress}" = {
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
              proxyPass = "http://localhost:${toString cfg.backend.port}";
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

      users.groups = {
        "${cfg.group}" = { };
      };

      users.users."${cfg.user}" = {
        description = "Give'n'take";
        group = "${cfg.group}";
        isSystemUser = true;
      };

      environment.systemPackages = [ flakePkgs.giveandtake-backend ];

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
          ${flakePkgs.giveandtake-backend}/bin/giveandtake ${configFile}
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
