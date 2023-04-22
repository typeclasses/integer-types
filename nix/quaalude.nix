{ mkDerivation, base, lib }:
mkDerivation {
  pname = "quaalude";
  version = "0.0.0.1";
  sha256 = "86d131bdffeb6032fe2ae1f04692f3ae732f2ddc9840ee0813fa1d2deacd9a73";
  revision = "1";
  editedCabalFile = "0mqgnxadwgz0ky3nvzqrkdnx5xb2a8qbiwvdwjmy63xhwdymy5ap";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/typeclasses/quaalude";
  description = "Extremely minimal prelude";
  license = lib.licenses.asl20;
}
