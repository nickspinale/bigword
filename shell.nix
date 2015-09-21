{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, mod-n }:
      mkDerivation {
        pname = "bigword";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base mod-n ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {
    mod-n = haskellPackages.callPackage ./mod-n {};
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
