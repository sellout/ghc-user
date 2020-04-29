with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskell.packages.ghc865.ghcWithPackages (p: with p; [
      cabal-install
    ]))
    (perl.withPackages (p: with p; [
      TemplateToolkit
    ]))
  ];
}
