with import <nixpkgs> { };

mkShell {
  buildInputs = with pkgs; [
    ghcid
    (haskellPackages.ghcWithPackages (p: [ p.kan-extensions ]))
  ];
}
