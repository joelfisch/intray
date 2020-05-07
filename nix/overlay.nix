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
      intrayPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName ( intrayPkg name );
      intrayPkgWithOwnComp = name: intrayPkgWithComp name name;
    in {

      "intray-api" = intrayPkg "intray-api";
      "intray-api-gen" = intrayPkg "intray-api-gen";
      "intray-cli" = intrayPkgWithComp "intray" "intray-cli";
      "intray-client" = intrayPkg "intray-client";
      "intray-data" = intrayPkg "intray-data";
      "intray-data-gen" = intrayPkg "intray-data-gen";
      "intray-server" = intrayPkgWithOwnComp "intray-server";
      "intray-server-gen" = intrayPkg "intray-server-gen";
      "intray-web-server" =
        let
          semantic-js =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js;
              sha256 =
                "sha256:0ll00jawcwd4nj568sj7lfp2ixrni9wqf37sz5nhz6wggjk9xhdp";
            };
          semantic-css =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css;
              sha256 =
                "sha256:0m13jdkv3vdqr0pbr1zfc2ndsafr2p5mnfzkbm7pd8v1ylwy8rpn";
            };
          jquery-js =
            builtins.fetchurl {
              url = https://code.jquery.com/jquery-3.1.1.min.js;
              sha256 =
                "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
            };
          icons-ttf =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf;
              sha256 =
                "sha256:1nm34hrh3inyrq7cbkh47g8m2hbqpsgkzbdrpfiiii7m8bsq2zyb";
            };
          icons-woff =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff;
              sha256 =
                "sha256:1qgzlmd80c4ckh9zpfl2qzjvg389hvmkdhkv8amyq4c71y2a9dlm";
            };
          icons-woff2 =
            builtins.fetchurl {
              url =
                https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2;
              sha256 =
                "sha256:1lqd60f1pml8zc93hgwcm6amkcy6rnbq3cyxqv5a3a25jnsnci23";
            };
          intrayAndroidRelease =
            let
              repo =
                final.fetchgit {
                  url =
                    "https://gitlab.com/Norfair/intray-android-release.git";
                  rev = "1df1b0d332f1f1326b4b29bc467c6f7671783b13";
                  sha256 =
                    "sha256:0jwf87w8j65vj1v0jxbn5mpa8pj3drwk31w1xy5fdbf9ddq10bgf";
                };
            in
              repo + "/app-release.apk";
        in
          overrideCabal ( intrayPkgWithOwnComp "intray-web-server" ) (
            old:
              {
                preConfigure =
                  ''
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
            cp ${intrayAndroidRelease} static/intray.apk
          '';
              }
          );
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions ( old.overrides or (_: _: {}) ) (
              self: super:
                let
                  typedUuidRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "typed-uuid";
                      rev = "dbc8fd4b56b78b1f9cf00bc2890d43dc19b97c5c";
                      sha256 =
                        "sha256:12b8na513xq5smlgwvqaqpplj8blfl452vdq0j2kv40qaw6y9qp7";
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
                      rev = "675ef7ef0b47b7021ea1bde1644e953a75d2c373";
                      sha256 =
                        "sha256:1rw0mifk5hwdrwd8van6rbxj8sgq2zsnhynlbxxxfj6759v65p1n";
                    };
                  typedUuidPkg =
                    name:
                      self.callCabal2nix name ( typedUuidRepo + "/${name}" ) {};
                  stripeHaskellPkg =
                    name:
                      dontCheck (
                        self.callCabal2nix name ( stripeHaskellRepo + "/${name}" ) {}
                      );
                  hsTlsRepo =
                    final.fetchFromGitHub {
                      owner = "ocheron";
                      repo = "hs-tls";
                      rev = "8d2201a7e73f826633a9537b98a24096252950cc";
                      sha256 =
                        "sha256:1ralh1hdqb6f135kqb6wmariv621w3q6vvdpwvyn4zrbfqjlxsx4";
                    };
                  hsTlsPkg =
                    name: subdir:
                      dontCheck (
                        self.callCabal2nix name ( hsTlsRepo + "/${subdir}" ) {}
                      );
                  hsTlsPackages =
                    {
                      "tls" = hsTlsPkg "tls" "core";
                      "tls-session-manager" =
                        hsTlsPkg "tls-session-manager" "session";
                      "tls-debug" = hsTlsPkg "tls-debug" "debug";
                    };

                in
                  {
            yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" yesodStaticRemoteRepo {});
            servant-auth-server = doJailbreak (super.servant-auth-server);
            looper = self.callCabal2nix "looper" looperRepo {};

          } // final.lib.genAttrs [
            "stripe-core"
            "stripe-haskell"
            "stripe-http-client"
            "stripe-http-streams"
          ] stripeHaskellPkg // final.lib.genAttrs [
            "typed-uuid"
            "genvalidity-typed-uuid"
          ] typedUuidPkg // hsTlsPackages // final.intrayPackages
            );
        }
    );
}
