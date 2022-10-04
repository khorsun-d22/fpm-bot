{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:edolstra/flake-compat";

    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.inputs.flake-utils.follows = "flake-utils";

    flake-compat.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , devshell
    , flake-compat
    }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "fpm-bot";
      preOverlays = [ devshell.overlay ];
      overlay = self: pkgs: {
        eggs =
          let
            eggs = import ./eggs.nix {
              inherit (pkgs) pkgs stdenv;
            };
          in
          eggs // {
            openssl = eggs.openssl.overrideAttrs (old: {
              buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.openssl ];
            });

            telebot = pkgs.chickenPackages.eggDerivation {
              name = "telebot";
              src = ./vendor/telebot;
              buildInputs = with self.eggs; [
                http-client
                medea
                openssl
                srfi-1
                srfi-133
                srfi-69
              ];
            };

            fpm-bot = pkgs.chickenPackages.eggDerivation {
              name = "fpm-bot";
              src = ./.;
              buildInputs = with self.eggs; [
                intarweb
                medea
                spiffy
                srfi-133
                srfi-69
                sxml-serializer
                telebot
                uri-common
              ];
            };
          };

        fpm-bot = {
          inherit (self.eggs) telebot fpm-bot;
        };
      };
      shell = { pkgs }:
        pkgs.devshell.mkShell {
          imports = [
            "${devshell}/extra/language/c.nix"
            "${devshell}/extra/locale.nix"
          ];
          extra.locale = {
            lang = "en_US.UTF-8";
          };
          devshell.packages = [
            pkgs.egg2nix
            pkgs.git-subrepo
            pkgs.rnix-lsp
            pkgs.nixpkgs-fmt
          ] ++ pkgs.fpm-bot.fpm-bot.buildInputs;
          language.c = {
            libraries = [ pkgs.openssl ];
            includes = [ pkgs.openssl ];
          };
          devshell.startup.setup-chicken-path.text = ''
            export CHICKEN_INSTALL_PREFIX="$HOME/fpm-bot/.local"
            export CHICKEN_INSTALL_REPOSITORY="''${CHICKEN_INSTALL_PREFIX}/lib/chicken/${toString pkgs.chicken.binaryVersion}"
            export CHICKEN_REPOSITORY_PATH="''${CHICKEN_INSTALL_REPOSITORY}:${pkgs.chicken}/lib/chicken/${toString pkgs.chicken.binaryVersion}"
            export PATH="''${CHICKEN_INSTALL_PREFIX}/bin:''${PATH}"
          '';
        };
    };
}
