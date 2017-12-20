{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, GLUT, OpenGL
      , stdenv
      }:
      mkDerivation {
        pname = "4d-solids-brutal";
        version = "0.1.0.0";
        src = ./4d-solids-brutal;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring containers GLUT OpenGL
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
