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
      sha256 = "0nyp4mmlz3hkppvaqzb9cr870cmhwzvcv7f2mjihyamzs3mjw5dn";
      rev = "fadd603d592cba8ae25c5be7d1e2b656ee75935b";
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
