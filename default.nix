let
  pkgs = import ./nix/pkgs.nix;
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/5c3078ad58856ce22f883b5518879d27bfc59dd5.tar.gz"
    );

in
  pkgs.intrayPackages //
{
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      canonix.enable = true;
      hindent.enable = true;
      hlint.enable = true;
    };
  };
}
