{ pkgs ? import <nixpkgs> { } }: 
let
  inherit (pkgs) haskellPackages;
  haskellDeps = ps: with ps; [
    hakyll
  ];
  ghc = haskellPackages.ghcWithPackages haskellDeps;
  nixPackages = [
    haskellPackages.cabal-install
    ghc
  ];
  project = haskellPackages.callCabal2nix "codeis-moe" ./. { };
in {
  shell = pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = with haskellPackages; [
      haskell-language-server
      ormolu
      hakyll
    ] ++ nixPackages;
  };
  project = project;
}
    

