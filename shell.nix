with import <nixpkgs> { };

mkShell {
  buildInputs = with pkgs; [
    ghcid
    (haskellPackages.ghcWithPackages (p: [
      p.aeson
      p.aeson-pretty
      p.constraints
      p.do-notation
      p.first-class-families
      p.indexed
      p.kan-extensions
      p.singletons
      p.strict
      p.vector
    ]))
  ];
}
