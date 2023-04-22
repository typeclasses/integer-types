{
  description = "integer-types";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }:
    let packageName = "integer-types";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgsUnstable = import nixpkgs-unstable { inherit system; };

        project = pkgs.haskellPackages.developPackage {
          root = ./integer-types;
          name = packageName;
        };

        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations = let

            inherit (pkgs.haskell.lib) dontCheck;
            makeTestConfiguration = let defaultPkgs = pkgs;
            in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            in (pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = combineOverrides old [
                (packageSourceOverrides { integer-types = ./integer-types; })
                (new: old: {
                  quaalude = dontCheck (new.callPackage ./nix/quaalude.nix { });
                })
                overrides
              ];
            })).integer-types;
          in rec {
            ghc-9-2 = makeTestConfiguration { ghcVersion = "ghc92"; };
            ghc-9-4 = makeTestConfiguration { ghcVersion = "ghc94"; };
            ghc-9-6 = makeTestConfiguration {
              ghcVersion = "ghc96";
              pkgs = pkgsUnstable;
              overrides = new: old: {
                hedgehog = dontCheck (new.callPackage ./nix/hedgehog.nix { });
                primitive = dontCheck (new.callPackage ./nix/primitive.nix { });
                quickcheck-classes-base = dontCheck
                  (new.callPackage ./nix/quickcheck-classes-base.nix { });
                tagged = dontCheck (new.callPackage ./nix/tagged.nix { });
              };
            };
            all = pkgs.symlinkJoin {
              name = "integer-types";
              paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ];
            };
          };
        };
      });
}
