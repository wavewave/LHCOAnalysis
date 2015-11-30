{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring
      , bytestring-lexing, LHCOAnalysis-type, mtl, split, stdenv, text
      }:
      mkDerivation {
        pname = "LHCOAnalysis";
        version = "1.0";
        src = ./.;
        libraryHaskellDepends = [
          attoparsec base bytestring bytestring-lexing LHCOAnalysis-type mtl
          split text
        ];
        description = "LHC Olympics File Analysis Code";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
