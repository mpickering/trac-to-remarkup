{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, connection
      , containers, data-default, directory, exceptions, filepath
      , http-client-tls, http-types, megaparsec, mtl
      , NoTrace, postgresql-simple, pretty-show, process, servant
      , servant-client, silently, stdenv, stm, text, time, transformers
      }:
      mkDerivation {
        pname = "trac-to-remarkup";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring containers megaparsec mtl NoTrace
          postgresql-simple process servant servant-client text time
        ];
        executableHaskellDepends = [
          async base bytestring connection containers data-default directory
          exceptions http-client-tls http-types mtl
          postgresql-simple servant-client stm text transformers
        ];
        testHaskellDepends = [
          base directory filepath pretty-show silently
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = with nixpkgs.haskell.lib; haskellPackages.callPackage f {
    NoTrace = doJailbreak haskellPackages.NoTrace;
    megaparsec = doJailbreak (haskellPackages.callHackage "megaparsec" "5.3.1" {
    });
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
