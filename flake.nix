{
  description = "GPipe - Typesafe functional GPU graphics programming (fork)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages;
        ghcWithPackages = hp.ghcWithPackages (p: [
          p.transformers
          p.exception-transformers
          p.containers
          p.Boolean
          p.hashtables
          p.gl
          p.linear
        ]);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghcWithPackages
            hp.cabal-install
            pkgs.pkg-config
            pkgs.libGL
            pkgs.libGLU
          ];
        };
      }
    );
}
