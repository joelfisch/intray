let pkgs = import ./default.nix;
in {
 intray-api               =    pkgs.haskellPackages.intray-api;
 intray-api-gen           =    pkgs.haskellPackages.intray-api-gen;
 intray-cli               =    pkgs.haskellPackages.intray-cli;
 intray-client            =    pkgs.haskellPackages.intray-client;
 intray-client-gen        =    pkgs.haskellPackages.intray-client-gen;
 intray-data              =    pkgs.haskellPackages.intray-data;
 intray-data-gen          =    pkgs.haskellPackages.intray-data-gen;
 intray-server            =    pkgs.haskellPackages.intray-server;
 intray-server-test-utils =    pkgs.haskellPackages.intray-server-test-utils;
 intray-web-server        =    pkgs.haskellPackages.intray-web-server;
}
