let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
in
  with nixpkgs;
  stdenv.mkDerivation {
    name = "moz_overlay_shell";
    buildInputs = [
      (nixpkgs.rustChannelOf { channel = "stable"; }).rust
      nixpkgs.curl
      nixpkgs.jq
      nixpkgs.sqlite
    ] ++ lib.optionals stdenv.isDarwin [
      nixpkgs.darwin.apple_sdk.frameworks.Security
      nixpkgs.darwin.apple_sdk.frameworks.CoreServices
    ];
  }
