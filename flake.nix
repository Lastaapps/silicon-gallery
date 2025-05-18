# https://docs.haskellstack.org/en/stable/topics/nix_integration/
# NEVER, I say NEVER try to use stack on NixOS, it is a mess.
{
  description = "Silicon Gallery";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # need to match Stackage LTS version from stack.yaml snapshot
        # hPkgs = pkgs.haskell.packages.ghc966;
        hPkgs = pkgs.haskellPackages;

        myDevTools = [
          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.ormolu
          hPkgs.hlint
          hPkgs.hoogle
          hPkgs.haskell-language-server
          hPkgs.implicit-hie
          hPkgs.retrie
          # hPkgs.cabal-install

          # hPkgs.stack
          stack-wrapped

          pkgs.zlib
          pkgs.zstd
        ];

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          env = {
            EXTRA_INCLUDE_DIRS = "${pkgs.zlib.dev}/include" + ":${pkgs.zstd.dev}/include";

            EXTRA_LIB_DIRS =
              "${pkgs.zlib.dev}/lib"
              + ":${pkgs.zlib.out}/lib"
              + ":${pkgs.zstd.dev}/lib"
              + ":${pkgs.zstd.out}/lib";

            NIX_PATH =
              "nixpkgs=${nixpkgs}"
              + (if builtins.getEnv "NIX_PATH" != "" then ":" + builtins.getEnv "NIX_PATH" else "");
          };

          shellHook = ''
            echo "EXTRA_INCLUDE_DIRS=$EXTRA_INCLUDE_DIRS"
            echo "EXTRA_LIB_DIRS=$EXTRA_LIB_DIRS"
          '';
        };
      }
    );
}
