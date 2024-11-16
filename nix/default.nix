{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

  makeTestConfiguration = { ghc ? pkgs.haskellPackages, overrides ? new: old: { } }:
    let
      inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
    in
    (ghc.override (old: {
      overrides =
        combineOverrides old [
          (packageSourceOverrides { integer-types = ../integer-types; })
          overrides
        ];
    })).integer-types;

  testConfigurations = rec {
    ghc-9-2 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc92;
    };
    ghc-9-4 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc94;
    };
    ghc-9-6 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc96;
    };
    ghc-9-8 = makeTestConfiguration {
      ghc = pkgs.haskell.packages.ghc98;
      overrides = new: old: {
        # x = new.callPackage ./haskell/x.nix { };
      };
    };
    all = pkgs.symlinkJoin {
      name = "integer-types-tests";
      paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ghc-9-8 ];
    };
  };

in
{

  packages = { inherit testConfigurations; };

  devShells.default = pkgs.mkShell {
    inputsFrom = [
      (makeTestConfiguration { }).env
    ];
    buildInputs = [
      pkgs.haskell-language-server
      pkgs.cabal-install
    ];
  };

}
