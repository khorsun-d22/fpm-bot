self: pkgs: {
  fpm-bot = import ./default.nix { inherit pkgs; };
}
