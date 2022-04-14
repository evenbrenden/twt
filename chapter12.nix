with import <nixpkgs> { };

mkShell {
  buildInputs = with pkgs; [
    ghcid
    (haskellPackages.ghcWithPackages (p: [ p.first-class-families p.vector ]))
  ];
}
