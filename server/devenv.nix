{ pkgs, ... }:

{
  services.postgres = {
    enable = true;
    package = pkgs.postgresql_15;
    listen_addresses = "127.0.0.1";
    initialDatabases = [ { name = "giveandtake"; } ];
    extensions = extensions: [ ];
  };

  services.nginx = {
    enable = true;
    httpConfig = ''
      # Load mime types.
      include /nix/store/c3xdbjc25q27x68swhkx1mfyw6vf5pc8-mailcap-2.1.53/etc/nginx/mime.types;
      # When recommendedOptimisation is disabled nginx fails to start because the mailmap mime.types database
      # contains 1026 entries and the default is only 1024. Setting to a higher number to remove the need to
      # overwrite it because nginx does not allow duplicated settings.
      types_hash_max_size 4096;
      include /nix/store/a6335limssh1yydi96yfaql1y0fd2lik-nginx-1.26.2/conf/fastcgi.conf;
      include /nix/store/a6335limssh1yydi96yfaql1y0fd2lik-nginx-1.26.2/conf/uwsgi_params;
      default_type application/octet-stream;
      # optimisation
      sendfile on;
      tcp_nopush on;
      tcp_nodelay on;
      keepalive_timeout 65;
      ssl_protocols TLSv1.2 TLSv1.3;
      ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:DHE-RSA-CHACHA20-POLY1305;
      gzip on;
      gzip_static on;
      gzip_vary on;
      gzip_comp_level 5;
      gzip_min_length 256;
      gzip_proxied expired no-cache no-store private auth;
      gzip_types application/atom+xml application/geo+json application/javascript application/json application/ld+json application/manifest+json application/rdf+xml application/vnd.ms-fontobject application/wasm application/x-rss+xml application/x-web-app-manifest+json application/xhtml+xml application/xliff+xml application/xml font/collection font/otf font/ttf image/bmp image/svg+xml image/vnd.microsoft.icon text/cache-manifest text/calendar text/css text/csv text/javascript text/markdown text/plain text/vcard text/vnd.rim.location.xloc text/vtt text/x-component text/xml;
      # $connection_upgrade is used for websocket proxying
      client_max_body_size 10m;
      server_tokens off;
      server {
      	listen 127.0.0.1:8090 ;
      	server_name localhost ;
      	location / {
      		proxy_pass http://localhost:3000;
      		proxy_http_version 1.1;
      		proxy_set_header Connection "upgrade";
      		proxy_set_header Upgrade $http_upgrade;
      		proxy_set_header X-Forwarded-For $remote_addr;
      		proxy_set_header X-Forwarded-Proto $scheme;
      	}
      	location /api {
      		proxy_pass http://localhost:8080;
      		proxy_http_version 1.1;
      		proxy_set_header Connection "upgrade";
      		proxy_set_header Upgrade $http_upgrade;
      		proxy_set_header X-Forwarded-For $remote_addr;
      		proxy_set_header X-Forwarded-Proto $scheme;
      	}
      	location /docs {
      		proxy_pass http://localhost:8000;
      		proxy_http_version 1.1;
      		proxy_set_header Connection "upgrade";
      		proxy_set_header Upgrade $http_upgrade;
      		proxy_set_header X-Forwarded-For $remote_addr;
      		proxy_set_header X-Forwarded-Proto $scheme;
      	}
      }
    '';
    # virtualHosts = {
    #   "localhost" = {
    #     listen = [
    #       {
    #         addr = "127.0.0.1";
    #         port = 8090;
    #       }
    #     ];
    #     locations."/" = {
    #       proxyPass = "http://localhost:3000";
    #       extraConfig = ''
    #         proxy_http_version 1.1;
    #         proxy_set_header Connection "upgrade";
    #         proxy_set_header Upgrade $http_upgrade;
    #         proxy_set_header X-Forwarded-For $remote_addr;
    #         proxy_set_header X-Forwarded-Proto $scheme;
    #       '';
    #     };
    #     locations."/api" = {
    #       proxyPass = "http://localhost:8080";
    #       extraConfig = ''
    #         proxy_http_version 1.1;
    #         proxy_set_header Connection "upgrade";
    #         proxy_set_header Upgrade $http_upgrade;
    #         proxy_set_header X-Forwarded-For $remote_addr;
    #         proxy_set_header X-Forwarded-Proto $scheme;
    #       '';
    #     };
    #     locations."/docs" = {
    #       proxyPass = "http://localhost:8000";
    #       extraConfig = ''
    #         proxy_http_version 1.1;
    #         proxy_set_header Connection "upgrade";
    #         proxy_set_header Upgrade $http_upgrade;
    #         proxy_set_header X-Forwarded-For $remote_addr;
    #         proxy_set_header X-Forwarded-Proto $scheme;
    #       '';
    #     };
    #   };
    # };
  };
}
