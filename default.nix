{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802", ...}:
let
  ghc = pkgs.haskell.packages."${compiler}".ghcWithPackages (pkgs: with pkgs;
    [ elm-bridge ]);
in pkgs.stdenv.mkDerivation {
  name = "gore-env";
  src = ./src;
  propagatedBuildInputs = [ ghc ] ++ (with pkgs; [ nodejs elmPackages.elm closurecompiler ]);
  closurecompiler = pkgs.closurecompiler;
  installPhase = ''
    mkdir -p $out
  '';
  shellHook = ''
    export PATH=$PATH:$(pwd)/node_modules/.bin
    npm install
  '';
}
