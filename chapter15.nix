with import <nixpkgs> { };

mkShell {
  buildInputs = with pkgs; [
    ghcid
    (haskellPackages.ghcWithPackages (p: [ p.aeson p.constraints p.singletons ]))
  ];
}
