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
        in {
          description = "Intray ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          environment =
            concatAttrs [
              {
                "INTRAY_WEB_SERVER_PORT" =
                  "${builtins.toString (cfg.web-port)}";
                "INTRAY_SERVER_HOST" =
                  "${builtins.toString (head cfg.api-hosts)}";
                "INTRAY_SERVER_PORT" = "${builtins.toString (cfg.api-port)}";
                "INTRAY_SERVER_LOG_LEVEL" = "${cfg.log-level}";
              }
              (
                optionalAttrs ( cfg.tracking-id != null ) {
                  "INTRAY_WEB_SERVER_ANALYTICS_TRACKING_ID" =
                    "${cfg.tracking-id}";
                }

              )
              (
                optionalAttrs ( cfg.verification-tag != null ) {
                  "INTRAY_WEB_SERVER_SEARCH_CONSOLE_VERIFICATION" =
                    "${cfg.verification-tag}";
                }
              )
              (
                optionalAttrs ( cfg.monetisation != null ) (
                  with cfg.monetisation;

                  {
                    "INTRAY_SERVER_STRIPE_PLAN" = "${stripe-plan}";
                    "INTRAY_SERVER_STRIPE_SECRET_KEY" =
                      "${stripe-secret-key}";
                    "INTRAY_SERVER_STRIPE_PUBLISHABLE_KEY" =
                      "${stripe-publishable-key}";
                  }
                )
              )
            ];
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${intray-pkgs.intray-web-server}/bin/intray-web-server \
                serve \
                --admin syd
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
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
