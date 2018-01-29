with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "haskell";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = with pkgs; [
    stack
  ];
  shellHook = "
  ";
}
