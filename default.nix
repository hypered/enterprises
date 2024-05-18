{
  compiler ? "ghc928"
  # 9.2.8 is the one corresponding to haskellPackages when not overlayed.
}:

let

  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix { inherit compiler; };
  nixpkgs = import sources.nixpkgs { inherit overlays; };

in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.enterprises;
    # binaries + haddock are also available as binaries.all.
    haddock = nixpkgs.haskellPackages.enterprises.doc;

    # A shell to try out our binaries
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
      ];
      shellHook = ''
        source <(enterprises --bash-completion-script `which enterprises`)
      '';
    };
  }
