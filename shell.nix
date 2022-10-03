{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = [
    pkgs.chicken
    pkgs.rnix-lsp
    pkgs.nixpkgs-fmt
    pkgs.openssl
  ];

  shellHook = ''
    export CHICKEN_INSTALL_PREFIX="$HOME/fpm-bot/.local"
    export CHICKEN_INSTALL_REPOSITORY="''${CHICKEN_INSTALL_PREFIX}/lib/chicken/5"
    export CHICKEN_REPOSITORY_PATH="''${CHICKEN_INSTALL_REPOSITORY}:''${CHICKEN_REPOSITORY_PATH}"
    export PATH="''${CHICKEN_INSTALL_PREFIX}/bin:''${PATH}"
  '';
}
