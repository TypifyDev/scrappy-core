{ mkDerivation, aeson, base, bytestring, containers, lens, lib
, modern-uri, network-uri, parsec, parser-combinators, text
, transformers, hspec
}:
mkDerivation {
  pname = "scrappy-core";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers lens modern-uri network-uri parsec
    parser-combinators text transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers lens modern-uri network-uri parsec
    parser-combinators text transformers
    hspec
  ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
