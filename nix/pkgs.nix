let
  pkgsv = import ( import ./nixpkgs.nix );
  pkgs = pkgsv {};
  validity-overlay =
    import (
      pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );

  pretty-relative-time-overlay =
    import (
      pkgs.fetchFromGitHub (import ./pretty-relative-time-version.nix) + "/nix/overlay.nix"
    );

  mergeless-overlay =
    import (
      pkgs.fetchFromGitHub (import ./mergeless-version.nix) + "/nix/overlay.nix"
    );

in
  pkgsv {
    overlays =
      [
        validity-overlay
        pretty-relative-time-overlay
        mergeless-overlay
        ( import ./gitignore-src.nix )
        ( import ./overlay.nix )
      ];
    config.allowUnfree = true;
  }
