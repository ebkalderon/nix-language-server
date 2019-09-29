# Source file comment

with import <nixpkgs> {};
let
  # Comment on binding
  #
  # Header
  #
  #   1. First
  #   2. Second
  set = { inherit (foo) test; foo.bar.baz = 12345; outer = { inner = true; }; };

  # Script for testing string interpolation
  value = "here\nare\rsome\trandom\tescape\"codes\$";
  script = pkgs.writeShellScriptBin "interpolationTest" ''
    #!/bin/bash
    echo '${value}'
  '';
in
  stdenv.mkDerivation {
    name = "interpolation-test";
    buildInputs = [ script ];
  }
