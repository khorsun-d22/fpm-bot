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
      overlay = ./overlay.nix;
      shell = { pkgs }: import ./shell.nix pkgs;
    };
}
