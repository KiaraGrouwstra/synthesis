{ cudaVersion ? null }:
let
  shared = import ./shared.nix { };
in
assert cudaVersion == null || cudaVersion == 9 || cudaVersion == 10;

if cudaVersion == null
then shared.shell-synthesis_cpu
else if cudaVersion == 9
  then shared.shell-synthesis_cudatoolkit_9_2
  else shared.shell-synthesis_cudatoolkit_10_1
