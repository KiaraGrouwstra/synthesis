{ghc ? null }:

let
  # Use pinned packages
  nixpkgs = with (builtins.fromJSON (builtins.readFile ./nix/src.json));
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/dbb9f8818af7cf2ea91b89f7d80a8fb1800cbfb5.tar.gz";
      sha256 = "1i8lsiscjzq066p02a5c7azjrvqxqkz6dipzkkyhh9rmhgs1aca3";
    };

  pkgs = import nixpkgs {};
in
  pkgs.haskell.lib.buildStackProject {
    # # Either use specified GHC or use GHC 8.6.4 (which we need for LTS 13.13)
    # ghc = if isNull ghc then pkgs.haskell.compiler.ghc864 else ghc;
    ghc = if isNull ghc then pkgs.haskell.compiler.ghc865 else ghc;
    extraArgs = "--system-ghc";
    name = "tf-env";
    buildInputs = with pkgs; [ snappy zlib protobuf libtensorflow ];
  }
