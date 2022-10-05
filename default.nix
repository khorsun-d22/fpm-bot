{ pkgs ? import <nixpkgs> { } }:
let self = {
  raw-eggs = import ./eggs.nix {
    inherit (pkgs) pkgs stdenv;
  };
  eggs = self.raw-eggs // {

    openssl = self.raw-eggs.openssl.overrideAttrs (old: {
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.openssl ];
    });

    telebot = pkgs.chickenPackages.eggDerivation {
      name = "telebot";
      src = ./vendor/telebot;
      buildInputs = builtins.attrValues {
        inherit (self.eggs)
          http-client
          medea
          openssl
          srfi-1
          srfi-133
          srfi-69;
      };
    };

    fpm-bot = pkgs.chickenPackages.eggDerivation {
      name = "fpm-bot";
      src = ./.;
      buildInputs = builtins.attrValues {
        inherit (self.eggs)
          http-client
          intarweb
          medea
          spiffy
          srfi-133
          srfi-69
          sxml-serializer
          telebot
          uri-common;
      };
    };

  };

  fpm-bot = self.eggs.fpm-bot.overrideAttrs (old: {
    name = "fpm-bot";
  });

  docker-image = pkgs.dockerTools.buildLayeredImage {
    name = "fpm-bot";
    tag = "latest";
    config.Entrypoint = [ "${self.fpm-bot}/bin/fpm-bot" ];
    config.Env = [
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      "QUOTES_FILE=${./quotes.txt}"
    ];
    config.WorkingDir = "/data";
    config.ExposedPorts."5000/tcp" = { };
    config.Volumes."/data" = { };
  };

}; in self
