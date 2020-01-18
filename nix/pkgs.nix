let
  pkgsv = import ( import ./nixpkgs.nix );
  pkgs = pkgsv {};
  validity-overlay =
    import (
      pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );

in
  pkgsv {
    overlays =
      [
        validity-overlay
        ( import ./gitignore-src.nix )
        ( import ./overlay.nix )
      ];
    config.allowUnfree = true;
  }
