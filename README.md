# Give and take 
Social network based on exchanging pictures

## Run
You can use the default nixos module of this flake.
``` nix
  services.giveandtake = {
    enable = true;
    user = "giveandtake";
    group = "giveandtake";
    baseUrl = "gat.valentin-herrmann.de";
    docsBaseUrl = "docsgat.valentin-herrmann.de";
    backend = {
      port = 8080;
      dbConfig = {
        connections = 10;
        connectionString = "postgresql://giveandtake@localhost/giveandtake?host=/run/postgresql";
        # use cfg.database.postgres.socket for socket name
      };
      timeout = 600;
      emailConfig = {
        smtpHost = "smtp.MAILDOMAIN";
        smtpUser = "gat@MAILDOMAIN";
        smtpPassFile = config.age.secrets.giveandtake-smtp-password.path;
      };
    };
  };
```
