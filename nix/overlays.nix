# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages,
#    if they become needed.

{
  compiler ? "ghc928"
}:

[ (import ./overlay.nix { inherit compiler; }) ]
