let pkgs = import ./default.nix;
in {
  packages =
    [
      pkgs.haskellPackages.intray-api
      pkgs.haskellPackages.intray-api-gen
      pkgs.haskellPackages.intray-cli
      pkgs.haskellPackages.intray-client
      pkgs.haskellPackages.intray-client-gen
      pkgs.haskellPackages.intray-data
      pkgs.haskellPackages.intray-data-gen
      pkgs.haskellPackages.intray-server
      pkgs.haskellPackages.intray-server-test-utils
      pkgs.haskellPackages.intray-web-server
    ];
}
