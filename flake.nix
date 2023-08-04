{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.ghc945;
      in {
        devShell = pkgs.mkShell {
	        buildInputs = [
	          hpkgs.haskell-language-server
	          pkgs.cabal-install
	          pkgs.haskell.compiler.ghc94

            pkgs.zlib
            pkgs.postgresql
	        ];
	    };
  });
}
