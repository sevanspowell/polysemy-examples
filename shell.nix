{ compiler ? "default"
, withHoogle ? true
, withProfiling ? false
}:

let
  inherit (import <nixpkgs> {}) fetchFromGitHub;

  pin-info = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pin-source = fetchFromGitHub {
    inherit (pin-info) owner repo rev sha256;
  };

  pkgs = import pin-source { overlays = [ (import ./overlay.nix {inherit compiler withHoogle withProfiling; }) ]; };

  drv =
    pkgs.haskell.lib.addBuildDepends
      (pkgs.haskellPackages.callCabal2nix "polysemy-examples" ./. {})
      (with pkgs.haskellPackages;
        [ apply-refact
          cabal-install
          cabal2nix
          ghcid
          hasktags
          hindent
          hlint
        ] ++ (with pkgs; [ git ])
      );
in
  if pkgs.lib.inNixShell then drv.env else drv
