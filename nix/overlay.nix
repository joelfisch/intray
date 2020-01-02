final: previous:
with final.haskell.lib;

{

  intrayPackages =
    let
      pathFor = name: final.gitignoreSource ( ../. + "/${name}" );
      intrayPkg =
        name:
          addBuildDepend (
            failOnAllWarnings (
              disableLibraryProfiling ( final.haskellPackages.callCabal2nix name ( pathFor name ) {} )
            )
          ) ( final.haskellPackages.autoexporter );
    in
      final.lib.genAttrs [
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
      ] intrayPkg // {
        "intray-web-server" =
        let semantic-js = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js;
              sha256 = "sha256:0ll00jawcwd4nj568sj7lfp2ixrni9wqf37sz5nhz6wggjk9xhdp";
            };
            semantic-css = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css;
              sha256 = "sha256:0m13jdkv3vdqr0pbr1zfc2ndsafr2p5mnfzkbm7pd8v1ylwy8rpn";
            };
            jquery-js = builtins.fetchurl {
              url = https://code.jquery.com/jquery-3.1.1.min.js;
              sha256 = "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
            };
            icons-ttf = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf;
              sha256 = "sha256:1nm34hrh3inyrq7cbkh47g8m2hbqpsgkzbdrpfiiii7m8bsq2zyb";
            };
            icons-woff = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff;
              sha256 = "sha256:1qgzlmd80c4ckh9zpfl2qzjvg389hvmkdhkv8amyq4c71y2a9dlm";
            };
            icons-woff2 = builtins.fetchurl {
              url = https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2;
              sha256 = "sha256:1lqd60f1pml8zc93hgwcm6amkcy6rnbq3cyxqv5a3a25jnsnci23";
            };
        in overrideCabal (intrayPkg "intray-web-server") (old: {
          preConfigure = ''
            ${old.preConfigure or ""}

            mkdir -p static/
            cp ${jquery-js} static/jquery.min.js
            mkdir -p static/semantic/
            cp ${semantic-css} static/semantic/semantic.min.css
            cp ${semantic-js} static/semantic/semantic.min.js
            mkdir -p static/semantic/themes/default/assets/fonts
            cp ${icons-ttf} static/semantic/themes/default/assets/fonts/icons.ttf
            cp ${icons-woff} static/semantic/themes/default/assets/fonts/icons.woff
            cp ${icons-woff2} static/semantic/themes/default/assets/fonts/icons.woff2
          '';
        });
      };
  haskellPackages =
    previous.haskellPackages.extend (
      self: super:
        let
          typedUuidRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "typed-uuid";
              rev = "4c5739c5e231b1cee6bd568ec55734116691ac8f";
              sha256 =
                "sha256:185ki38vyvq5889vqdsw53dcdwssdyl4rzvxfhh6kbby17x2f835";
            };
          mergelessRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "mergeless";
              rev = "3d5f4b54cc2c4c8c6f33a716bc6b67f376b8d1d5";
              sha256 =
                "sha256:0far86wdprvyk8i50y4i5wzc0vcqj5ksdf90jnyyalrbklgxxgkv";
            };
          prettyRelativeTimeRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "pretty-relative-time";
              rev = "2abdb2ba83ad47b8369e2ef618c7c21b82d80b23";
              sha256 =
                "sha256:13csx5p0y7wsf8w1vzi3h3bnm4s5976rw9r9glhi55xdwq9s65q7";
            };
          stripeHaskellRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "stripe";
              rev = "7ced8cef1e932d3fb222dfb3c79c25595cdc82ab";
              sha256 =
                "sha256:04dsfx568hmmrr7zg5gbqwipdiy7lvpckfk2ayln6gh6zf9jxl13";
            };
          yesodStaticRemoteRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "yesod-static-remote";
              rev = "22c0a92c1d62f1b8d432003844ef0636a9131b08";
              sha256 =
                "sha256:1mz1fb421wccx7mbpn9qaj214w4sl4qali5rclx9fqp685jkfj05";
            };
          looperRepo =
            final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "looper";
              rev = "929a8ad6a99a84624767bd9d619cc5318c6bda56";
              sha256 = "07wc2as7p2pz08a9qfx2jm3kz1cvfg73d872il3zhyplbd6yhzbx";
            };
          typedUuidPkg =
            name:
              super.callCabal2nix name ( typedUuidRepo + "/${name}" ) {};
          validityPkg =
            name:
              super.callCabal2nix name ( validityRepo + "/${name}" ) {};
          mergelessPkg =
            name:
              super.callCabal2nix name ( mergelessRepo + "/${name}" ) {};
          stripeHaskellPkg =
            name:
              dontCheck (
                super.callCabal2nix name ( stripeHaskellRepo + "/${name}" ) {}
              );
        in
          {
            pretty-relative-time = super.callCabal2nix "pretty-relative-time" prettyRelativeTimeRepo {};
            yesod-static-remote = dontCheck (super.callCabal2nix "yesod-static-remote" yesodStaticRemoteRepo {});
            servant-auth-server = doJailbreak (super.servant-auth-server);
            looper = super.callCabal2nix "looper" looperRepo {};

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
          ] mergelessPkg // final.intrayPackages
    );
}
