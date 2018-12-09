{config, pkgs, lib, ...}:

let

  cfg = config.services.sewingPatternEditor;

  server = pkgs.buildGoPackage {
    name = "sewing-patter-editor-server";
    src = pkgs.fetchgit {
      url = "git://github.com/kirchner/sewing-pattern-editor.git";
      rev = "2ddb54043896af9e723ea75a54ad1445f1f530a7";
      sha256 = "0lm3kll70g0fk0al7s4qvpb26kq0l1sqbic1mwyv5k2k7nrkzn2d";
    };
    goPackagePath = "github.com/kirchner/sewing-pattern-editor";
  };

  dist = pkgs.fetchzip {
    url = "https://github.com/kirchner/sewing-pattern-editor/releases/download/v0.1/dist.tar.gz";
    sha256 = "1f3jaibvm06c5iia8dh37pvyxiw4b2nkxxjxpkfr7kwq3dhijccg";
  };

  sewingPatternEditorEnvironment = {
    SEWING_PATTERN_EDITOR_DIST_PATH = "${dist}";
    SEWING_PATTERN_EDITOR_PORT = "${toString cfg.port}";
  };

in

with lib;

{
  options = {
    services.sewingPatternEditor = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Start the sewing-pattern-editor server.
        '';
      };
      port = mkOption {
        default = 8080;
        type = types.int;
        description = ''
          The port to be used.
        '';
      };
      user = mkOption {
        default = "sewing-pattern-editor";
        type = types.string;
        description = ''
          The user under which the server should run.
        '';
      };
      group = mkOption {
        default = "sewing-pattern-editor";
        type = types.string;
        description = ''
          The group under which the server should run.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    users.users = [
      { name = cfg.user;
        group = cfg.group;
        home = "/tmp";
        shell = "${pkgs.bash}/bin/bash";
      }
    ];

    users.groups = [
      { name = cfg.group;
      }
    ];

    systemd.services.sewingPatternEditor = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "Start the sewing-pattern-editor server.";
      environment = sewingPatternEditorEnvironment;
      serviceConfig = {
        User = "sewing-pattern-editor";
        Group = "sewing-pattern-editor";
        ExecStart = ''${server}/bin/sewing-pattern-editor'';
      };
    };

    environment.systemPackages = [ pkgs.screen ];
  };
}
