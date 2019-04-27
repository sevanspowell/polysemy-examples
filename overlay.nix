{ compiler, withHoogle, withProfiling }:

self: super:

let
  hsPkgs = if compiler == "default"
           then super.haskellPackages
           else super.haskell.packages.${compiler};

  addHoogleOverlay = overlay:
                       self.lib.composeExtensions
                         overlay
                         (hself: hsuper: {
                           ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
                           ghcWithPackages = hself.ghc.withPackages;
                         });

  addProfilingOverlay = overlay:
                          self.lib.composeExtensions
                            overlay
                            (hself: hsuper: {
                              mkDerivation = args: hsuper.mkDerivation (args // {
                                enableLibraryProfiling = true;
                              });
                            });

  maybeAddHoogle = if withHoogle
                   then addHoogleOverlay
                   else self.lib.id;

  maybeAddProfiling = if withProfiling
                      then addProfilingOverlay
                      else self.lib.id;

  hsPkgSources = {
    polysemy = self.pkgs.fetchFromGitHub {
      owner = "isovector";
      repo = "polysemy";
      sha256 = "19nkhbwp52cw8avcc7v625cnp7rn6ngr25xalzcarm6h916s0dgh";
      rev = "d886b32fe421d8fdca0da7c3f1188a60973f676a";
    };
  };

  hsPkgsOverlay = hself: hsuper: {
    polysemy = hself.callCabal2nix "polysemy" "${hsPkgSources.polysemy}" {};
  };

in
  {
    haskellPackages = hsPkgs.override (oldArgs: {
      overrides =
        self.lib.composeExtensions
          (oldArgs.overrides or (_: _: {}))
          (maybeAddProfiling (maybeAddHoogle hsPkgsOverlay));
    });

  }
