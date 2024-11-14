{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "codeismoe-github-io" ./. {}

