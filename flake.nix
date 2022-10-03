{
  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-unstable";
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
    let
      maybeDev = p:
        if p ? dev
        then p.dev
        else p;
      makeIncludePath = ps:
        nixpkgs.lib.makeSearchPath "include"
          (map maybeDev ps);
      makePkgconfigPath = ps:
        nixpkgs.lib.makeSearchPath "lib/pkgconfig"
          (map maybeDev ps);
    in
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "fpm-bot";
      preOverlays = [ devshell.overlay ];
      overlay = self: pkgs: {
        fpm-bot = {
          fpm-bot = pkgs.chickenPackages.eggDerivation {
            name = "fpm-bot";
            src = ./.;
            buildInputs = [ ];
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
            pkgs.git-subrepo
            pkgs.chicken
            pkgs.rnix-lsp
            pkgs.nixpkgs-fmt
          ];
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
