{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.intray."${envname}";
  concatAttrs = attrList: fold ( x: y: x // y ) {} attrList;
in {
  options.services.intray."${envname}" =
    {
      enable = mkEnableOption "Intray Service";
      web-hosts =
        mkOption {
          type = types.listOf ( types.string );
          example = "intray.cs-syd.eu";
          description = "The host to serve web requests on";
        };
      api-hosts =
        mkOption {
          type = types.listOf ( types.string );
          example = "api.intray.cs-syd.eu";
          description = "The host to serve API requests on";
        };
      web-port =
        mkOption {
          type = types.int;
          default = 8000;
          example = 8000;
          description = "The port to serve web requests on";
        };
      api-port =
        mkOption {
          type = types.int;
          default = 8001;
          example = 8001;
          description = "The port to serve API requests on";
        };
      log-level =
        mkOption {
          type = types.string;
          default = "LevelWarning";
          example = "LevelInfo";
          description = "The log level";
        };
      tracking-id =
        mkOption {
          type = types.nullOr types.string;
          default = null;
          example = "UA-53296133-1";
          description = "The tracking id for google analytics";
        };
      verification-tag =
        mkOption {
          type = types.nullOr types.string;
          default = null;
          example = "ADkAx2F-JQO9KJBBdLfAGuJ_OMqPOsX5MdGDsfd0Ggw";
          description = "The verification tag for google search console";
        };
      admins =
        mkOption {
          type = types.nullOr ( types.listOf types.string );
          default = null;
          example = [ "syd" ];
          description =
            "A list of the usernames that will have admin privileges";
        };
      monetisation =
        mkOption {
          default = null;
          type =
            types.nullOr (
              types.submodule {
                options =
                  {
                    stripe-plan =
                      mkOption {
                        type = types.string;
                        example = "plan_XXXXXXXXXXXXXX";
                        description = "Stripe plan for subscriptions.";
                      };
                    stripe-secret-key =
                      mkOption {
                        type = types.string;
                        example = "sk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe secret key.";
                      };
                    stripe-publishable-key =
                      mkOption {
                        type = types.string;
                        example = "pk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe publishable key.";
                      };
                  };
              }
            );
        };
    };
  config =
    let
      intray-service =
        let
          workingDir = "/www/intray/${envname}/data/";
          intray-pkgs =
            (import ./pkgs.nix).intrayPackages;
          configFile =
            let
              config =
                {
                  api-host = head cfg.api-hosts;
                  api-port = cfg.api-port;
                  admins = cfg.admins;
                  log-level = cfg.log-level;
                  monetisation =
                    optionalAttrs ( !builtins.isNull cfg.monetisation ) {
                      stripe-plan = cfg.monetisation.stripe-plan;
                      stripe-secret-key = cfg.monetisation.stripe-secret-key;
                      stripe-publishable-key =
                        cfg.monetisation.stripe-publishable-key;
                    };
                  web-port = cfg.web-port;
                  tracking = cfg.tracking-id;
                  verification = cfg.verification-tag;
                };
            in
              pkgs.writeText "intray-config" ( builtins.toJSON config );
        in {
          description = "Intray ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${intray-pkgs.intray-web-server}/bin/intray-web-server serve  --config-file ${configFile}
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };

        };
    in
      mkIf cfg.enable {
        systemd.services =
          {
            "intray-${envname}" = intray-service;
          };
        networking.firewall.allowedTCPPorts = [ cfg.web-port cfg.api-port ];
        services.nginx.virtualHosts =
          {
            "${head (cfg.web-hosts)}" =
              {
                enableACME = true;
                forceSSL = true;
                locations."/".proxyPass =
                  "http://localhost:${builtins.toString (cfg.web-port)}";
                serverAliases = tail cfg.web-hosts;
              };
            "${head (cfg.api-hosts)}" =
              {
                enableACME = true;
                forceSSL = true;
                locations."/".proxyPass =
                  "http://localhost:${builtins.toString (cfg.api-port)}";
                serverAliases = tail cfg.api-hosts;
              };
          };
      };
}
