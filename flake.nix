{
  description = "hasql-migration library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        hasql-migration = pkgs.haskellPackages.callCabal2nix "hasql-migration" (./.) {};
      in {
        packages.default = hasql-migration;
        devShells.default = pkgs.haskellPackages.shellFor rec {
          packages = hpkgs: [];
          buildInputs = with pkgs; [
            nil
            just
            nixfmt-rfc-style
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.ormolu
            haskellPackages.haskell-language-server
            # This is all to get Haskell's zlib to compile
            zlib
            zstd
            xz
            bzip2
            # We need libpq for hasql
            libpq
          ];
          # Needed to get Haskell's zlib to compile
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };
      }
    );
}
