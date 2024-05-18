{
  compiler ? "ghc928"
}:

self: super:
let

  lib = super.lib;
  contents = import ./contents.nix { nixpkgs = super; };

in {
  haskellPackages = super.haskell.packages."${compiler}".extend (
    lib.foldl' (lhs: rhs: lib.composeExtensions lhs rhs) (_: _: {}) [contents.overrides]
  );
}
