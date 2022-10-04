{ pkgs ? import <nixpkgs> { }
, devshell ? import
    (fetchTarball {
      url = "https://github.com/numtide/devshell/archive/e3dc3e21594fe07bdb24bdf1c8657acaa4cb8f66.tar.gz";
      sha256 = "sha256:040qai0qkf443w410hx8kgnvay000kanqjglsrcbbixlmrq6a5gv";
    })
    { nixpkgs = pkgs; }
, ...
}:
let fpm-bot = import ./default.nix { inherit pkgs; };
in
devshell.mkShell {
  imports = [
    "${devshell.extraModulesDir}/language/c.nix"
    "${devshell.extraModulesDir}/locale.nix"
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
            (map (egg: "${egg}/${chicken-repo-suffix}")
              (builtins.filter pkgs.lib.isDerivation
                ([ pkgs.chicken ] ++ builtins.attrValues fpm-bot.eggs)));
      }
    ];
}
