let
  shared = import nix/shared.nix { };
in
  { inherit (shared) synthesis_cpu synthesis-docs;
    ${shared.nullIfDarwin "synthesis_cudatoolkit_9_2"} = shared.synthesis_cudatoolkit_9_2;
    ${shared.nullIfDarwin "synthesis_cudatoolkit_10_1"} = shared.synthesis_cudatoolkit_10_1;
  }
