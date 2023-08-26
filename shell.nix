{ }:

let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-21.11";
    url = "https://github.com/nixos/nixpkgs/archive/8e6b3914626900ad8f465c3e3541edbb86a95d41.tar.gz";
    sha256 = "02smga9l709cg3almbjk1jf6jnl1x5lnqwv0jz6d5rn83c3n7dc0";
  }) { };
in pkgs.mkShell {
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
