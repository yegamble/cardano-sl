let
  # Allow overriding pinned nixpkgs for debugging purposes via cardano_pkgs
  fetchNixPkgs = let try = builtins.tryEval <cardano_pkgs>;
    in if try.success
    then builtins.trace "using host <cardano_pkgs>" try.value
    else import ./fetch-nixpkgs.nix;

  maybeEnv = env: default:
    let
      result = builtins.getEnv env;
    in if result != ""
       then result
       else default;

  cleanHaskellSource = src:
    if (builtins.typeOf src) == "path"
      then lib.cleanSourceWith {
        filter = with pkgs.stdenv;
          name: type: let baseName = baseNameOf (toString name); in ! (
            # Filter out cabal build products
            baseName == "dist" ||
            # Filter out files which don't affect cabal build
            lib.hasSuffix ".nix" baseName
          );
        src = lib.cleanSource src;
      } else src;

  pkgs = import fetchNixPkgs {};
  lib = pkgs.lib;
in lib // (rec {
  inherit fetchNixPkgs cleanHaskellSource;
  isCardanoSL = lib.hasPrefix "cardano-sl";
  isBenchmark = args: !((args.isExecutable or false) || (args.isLibrary or true));
})
