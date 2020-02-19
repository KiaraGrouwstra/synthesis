FROM nixos/nix
# RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
# RUN nix-channel --update
COPY . /app
WORKDIR /app
RUN nix-build
