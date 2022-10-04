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
          inherit (self) eggs;

          fpm-bot = self.eggs.fpm-bot.overrideAttrs (old: {
            name = "fpm-bot";
          });

          docker-image = pkgs.dockerTools.buildLayeredImage {
            name = "fpm-bot";
            tag = "latest";
            config.Cmd = [ "${self.fpm-bot.fpm-bot}/bin/fpm-bot" ];
            config.ExposedPorts = { "8080" = { }; };
          };

          eggsToRepo = name: eggs: pkgs.symlinkJoin {
            inherit name;
            paths = eggs;
          };
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
            pkgs.chicken
            pkgs.egg2nix
            pkgs.git-subrepo
            pkgs.rnix-lsp
            pkgs.nixpkgs-fmt
          ];
          language.c = {
            libraries = [ pkgs.openssl ];
            includes = [ pkgs.openssl ];
          };
          env =
            let chicken-repo-suffix = "lib/chicken/${toString pkgs.chicken.binaryVersion}";
            in
            [
              { name = "CHICKEN_INSTALL_PREFIX"; eval = "$PRJ_DATA_DIR/local"; }
              { name = "CHICKEN_INSTALL_REPOSITORY"; eval = "$CHICKEN_INSTALL_PREFIX/${chicken-repo-suffix}"; }
              {
                name = "CHICKEN_REPOSITORY_PATH";
                value =
                  pkgs.lib.concatStringsSep ":"
                    (map (egg: "${egg}/${chicken-repo-suffix}") pkgs.fpm-bot.fpm-bot.propagatedBuildInputs);
              }
            ];
        };
    };
}
