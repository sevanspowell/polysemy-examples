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
      sha256 = "148pbpivpka2dlnym98dws2njli1v4f0zq8bq4rklrfzdraayd9a";
      rev = "ac6d7b312114863987f103871df54b2a5d1fe7d8";
    };

    type-errors = self.pkgs.fetchFromGitHub {
      owner = "isovector";
      repo = "type-errors";
      sha256 = "1pkrns559mh1psp2ic4dybk8x91vcx04xpjx7m7msqx7xypfz2il";
      rev = "f171628f607d83de18bd96c1d4247674fece6b15";
    };

    type-errors-pretty = self.pkgs.fetchFromGitHub {
      owner = "chshersh";
      repo = "type-errors-pretty";
      sha256 = "1z8ad658ap26110hnjc2iq30das45nqw8a969kqpmzl8568qgh0b";
      rev = "93f7f90c6b956bd5ab59645746c4a78e3f8d1676";
    };

    first-class-families = self.pkgs.fetchFromGitHub {
      owner = "Lysxia";
      repo = "first-class-families";
      sha256 = "1jvn11nbb7271hhy7q21cyn4r238pfzj00aabd39gwhi0g2gzqbf";
      rev = "2fb77468d3b0ce64dc5371bed1b636d60219975c";
    };

    th-abstraction = self.pkgs.fetchFromGitHub {
      owner = "glguy";
      repo = "th-abstraction";
      sha256 = "0z2rfv07h3xfsp45sqsh7ddhh26ph6qhwjlh2b10459k1pmpd7bw";
      rev = "855efbfdc0972094465492e1c862f760c068954d";
    };
  };

  hsPkgsOverlay = hself: hsuper: {
    # polysemy = hself.callCabal2nix "polysemy" "${hsPkgSources.polysemy}" {};
    # type-errors = hself.callCabal2nix "type-errors" "${hsPkgSources.type-errors}" {};
    # type-errors-pretty = hself.callCabal2nix "type-errors-pretty" "${hsPkgSources.type-errors-pretty}" {};
    # first-class-families = hself.callCabal2nix "first-class-families" "${hsPkgSources.first-class-families}" {};
    # xyz = hself.callCabal2nix "xyz" "${hsPkgSources.xyz}" {};
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
