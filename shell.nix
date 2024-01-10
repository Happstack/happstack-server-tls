{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, extensible-exceptions
      , happstack-server, hslogger, HsOpenSSL, network, openssl, sendfile
      , stdenv, time, unix, cabal-install
      }:
      mkDerivation {
        pname = "happstack-server-tls";
        version = "7.1.6";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring extensible-exceptions happstack-server hslogger
          HsOpenSSL network sendfile time unix
        ];
        librarySystemDepends = [ openssl ];
        homepage = "http://www.happstack.com/";
        description = "extend happstack-server with https:// support (TLS/SSL)";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
