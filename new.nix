let
    nixpkgs = import <nixpkgs> {};
    haskTorchSrc = builtins.fetchGit {
        url = https://github.com/hasktorch/hasktorch;
        rev = "dc62f1e1152f9e7fa66f7f9e78f385514715b816";
        ref = "master";
    };
    hasktorchOverlay = (import (haskTorchSrc + "/nix/shared.nix") { compiler = "ghc865"; }).overlayShared;
    haskellOverlay = cabal2nix;
    pkgs = import nixpkgs {overlays = [ hasktorchOverlay haskellOverlay ]; config={allowUnfree=true; allowBroken=true;};};
    # nixpkgsPath -> nixpkgs
in
pkgs
