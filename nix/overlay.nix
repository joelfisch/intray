final:
  previous:
    with final.haskell.lib;
    {
      intrayPackages = let
        pathFor = name:
          builtins.path {
            inherit name;
            path = ../. + "/${name}";
            filter = path:
              type:
                !final.lib.hasPrefix "." (baseNameOf path);
          };
        intrayPkg = name:
          failOnAllWarnings (disableLibraryProfiling (final.haskellPackages.callCabal2nix name (pathFor name) {}));
      in final.lib.genAttrs [
        "intray-data"
        "intray-data-gen"
        "intray-api"
        "intray-api-gen"
        "intray-cli"
        "intray-client"
        "intray-data"
        "intray-data-gen"
        "intray-server"
        "intray-server-test-utils"
        "intray-web-server"
      ] intrayPkg;
      haskellPackages = previous.haskellPackages.extend (self:
        super:
          let
            typedUuidRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "typed-uuid";
              rev = "155c9ec880ca1c12f7dd8a8468b3626de8164823";
              sha256 = "0wvdj07vhd7q93f7sdg4mq8f9nk4w3fjsq3z7nx7zm5dv0j78iwb";
            };
            mergelessRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "mergeless";
              rev = "cd1194d9abfa65366e3496e78a3eb5df3b36de56";
              sha256 = "1l3n32p732z6l3xjhv3ml0my0d0skpy7815lkf66ralp0sjcvc4c";
            };
            prettyRelativeTimeRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "pretty-relative-time";
              rev = "85ee8a577fb576e2dd7643bf248ff8fbbe9598ec";
              sha256 = "0g7i1nb52l41ka99z4s8ylm9w7jxmihr5s7rcp6kb7splmg99i6h";
            };
            typedUuidPkg = name:
              super.callCabal2nix name (typedUuidRepo + "/${name}") {};
            validityPkg = name:
              super.callCabal2nix name (validityRepo + "/${name}") {};
            mergelessPkg = name:
              super.callCabal2nix name (mergelessRepo + "/${name}") {};
          in {
            pretty-relative-time = super.callCabal2nix "pretty-relative-time" prettyRelativeTimeRepo {};
            servant-auth-server = doJailbreak (super.servant-auth-server);
          } // final.lib.genAttrs [
            "typed-uuid"
            "genvalidity-typed-uuid"
          ] typedUuidPkg // final.lib.genAttrs [
            "mergeless"
            "genvalidity-mergeless"
          ] mergelessPkg // final.intrayPackages);
    }
