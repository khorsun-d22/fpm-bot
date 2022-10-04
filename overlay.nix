self: pkgs: {
  fpm-bot = {

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
          buildInputs = with self.fpm-bot.eggs; [
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
          buildInputs = with self.fpm-bot.eggs; [
            http-client
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

    fpm-bot = self.fpm-bot.eggs.fpm-bot.overrideAttrs (old: {
      name = "fpm-bot";
    });

    docker-image = pkgs.dockerTools.buildLayeredImage {
      name = "fpm-bot";
      tag = "latest";
      config.Cmd = [ "${self.fpm-bot.fpm-bot}/bin/fpm-bot" ];
      config.ExposedPorts = { "8080" = { }; };
    };

  };
}
