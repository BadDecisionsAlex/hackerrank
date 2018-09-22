{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc843"
}:
let

  hp = with pkgs.haskellPackages; [
    text
  ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; hp);

in
  pkgs.stdenv.mkDerivation {
    name = "my-haskell-environment";
    buildInputs = [ ghc ] ++ hp ++ (with pkgs; [
      
    ]);
  shellHook = ''
    ghc --version
  '';
}
