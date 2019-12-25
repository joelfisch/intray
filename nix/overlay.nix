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
              rev = "0198f9393e2ef71f26de6427541387be44fd499b";
              sha256 = "1699fj3w5ydigvhm0cixblr1fg4fzxbx0m3l6fr5v1dcn589sbpa";
            };
            prettyRelativeTimeRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "pretty-relative-time";
              rev = "21b29c9d729ed91a56819f569de6fdc8582d7e3d";
              sha256 = "15ash7kdr03d37hx1s3hiwpms969j3vnqxji13q5wqj47nwiqj14";
            };
            stripeHaskellRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "stripe";
              rev = "ab23e8d5a7232d81d818095fad3fd361fbd485dd";
              sha256 = "sha256:1574ns3f547b7aa921q13kwaqv9dnr6q6fm2gp2ysh2ssj4pbgl6";
            };
            typedUuidPkg = name:
              super.callCabal2nix name (typedUuidRepo + "/${name}") {};
            validityPkg = name:
              super.callCabal2nix name (validityRepo + "/${name}") {};
            mergelessPkg = name:
              super.callCabal2nix name (mergelessRepo + "/${name}") {};
            stripeHaskellPkg = name:
              dontCheck (super.callCabal2nix name (stripeHaskellRepo + "/${name}") {});
          in {
            pretty-relative-time = super.callCabal2nix "pretty-relative-time" prettyRelativeTimeRepo {};
            servant-auth-server = doJailbreak (super.servant-auth-server);
          } // final.lib.genAttrs [
            "stripe-core"
            "stripe-haskell"
            "stripe-http-client"
            "stripe-http-streams"
          ] stripeHaskellPkg // final.lib.genAttrs [
            "typed-uuid"
            "genvalidity-typed-uuid"
          ] typedUuidPkg // final.lib.genAttrs [
            "mergeless"
            "genvalidity-mergeless"
          ] mergelessPkg // final.intrayPackages);
    }
