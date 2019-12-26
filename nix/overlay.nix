final:
  previous:
    with final.haskell.lib;
    {
      intrayPackages = let
        pathFor = name: final.gitignoreSource (../. + "/${name}");
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
              rev = "4c5739c5e231b1cee6bd568ec55734116691ac8f";
              sha256 = "sha256:185ki38vyvq5889vqdsw53dcdwssdyl4rzvxfhh6kbby17x2f835";
            };
            mergelessRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "mergeless";
              rev = "3d5f4b54cc2c4c8c6f33a716bc6b67f376b8d1d5";
              sha256 = "sha256:0far86wdprvyk8i50y4i5wzc0vcqj5ksdf90jnyyalrbklgxxgkv";
            };
            prettyRelativeTimeRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "pretty-relative-time";
              rev = "2abdb2ba83ad47b8369e2ef618c7c21b82d80b23";
              sha256 = "sha256:13csx5p0y7wsf8w1vzi3h3bnm4s5976rw9r9glhi55xdwq9s65q7";
            };
            stripeHaskellRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "stripe";
              rev = "ab23e8d5a7232d81d818095fad3fd361fbd485dd";
              sha256 = "sha256:1574ns3f547b7aa921q13kwaqv9dnr6q6fm2gp2ysh2ssj4pbgaa";
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
