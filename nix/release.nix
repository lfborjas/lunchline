let
  pkgs = import ./packages.nix {};
in
  { lunchline = pkgs.haskellPackages.lunchline; }
