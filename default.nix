{ mkDerivation, base, bytestring, extensible-exceptions
, happstack-server, hslogger, HsOpenSSL, network, openssl, sendfile
, stdenv, time, unix
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
}
