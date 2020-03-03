#!/usr/bin/env bash

nixos-container create sewinghub --config "
  networking.firewall.allowedTCPPorts = [ 5432 ];
  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 10.233.0.0/16 trust
    '';
    initialScript = pkgs.writeText \"init\" ''
      CREATE USER postgres;
      ALTER USER postgres PASSWORD 'postgres';
      ALTER USER postgres WITH SUPERUSER;
    '';
  };
"
