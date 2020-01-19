{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.intray;


in {
  options =
    {
      programs.intray =
        {
          enable = mkEnableOption "Intray cli";
          cache-dir =
            mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "The cache dir";
            };
          data-dir =
            mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "The data dir";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Intray synchronisation";
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description = "The username to use for syncing";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            description = "The password to use for syncing";
                          };
                        url =
                          mkOption {
                            type = types.str;
                            default = "https://api.intray.eu";
                            description =
                              "The sync server to use for syncing";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      intrayPkgs = import ./nix/pkgs.nix;

      nullOrOption =
        name: opt: optionalString ( opt != null ) "${name}: ${opt}";
      syncConfig =
        optionalString ( cfg.sync != null ) ''
        url: '${cfg.sync.url}'
        username: '${cfg.sync.username}'
        sync: NeverSync
      '';
      configFileContents =
        ''
        ${nullOrOption "cache-dir" cfg.cache-dir}
        ${nullOrOption "data-dir" cfg.data-dir}
        ${syncConfig}
      '';

      cli = intrayPkgs.intray-cli;

      syncIntrayName = "sync-intray";
      syncIntrayService =
        {
          Unit =
            {
              Description = "Sync intray items";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "intray-sync" ''
              ${cli}/bin/intray login --password "${cfg.sync.password}"
              ${cli}/bin/intray sync
          ''}";
              Type = "oneshot";
            };
        };

      syncIntrayTimer =
        {
          Unit =
            {
              Description = "Sync intray items every five minutes";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*:0/5";
              Persistent = true;
              Unit = "${syncIntrayName}.service";
            };
        };
      packages = [ cli ];
      services =
        optionalAttrs ( cfg.sync != null && cfg.sync.enable ) {
          "${syncIntrayName}" = syncIntrayService;
        };
      timers =
        optionalAttrs ( cfg.sync != null && cfg.sync.enable ) {
          "${syncIntrayName}" = syncIntrayTimer;
        };
    in
      mkIf cfg.enable {
        xdg.configFile."intray/config.yaml".text = configFileContents;
        systemd.user =
          {
            services = services;
            timers = timers;
          };
        home.packages = [ cli ];
      };
}
