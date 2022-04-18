with import <nixpkgs> { };

mkShell {
  buildInputs = with pkgs; [
    ghcid
    (haskellPackages.ghcWithPackages
      (p: [ p.do-notation p.first-class-families p.indexed p.strict ]))
  ];
}
