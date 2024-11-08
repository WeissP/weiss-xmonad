{
  description = "weiss-xmonad";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
  };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  outputs =
    inputs@{
      self,
      flake-utils,
      haskellNix,
      devshell,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
      ]
      (
        system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev: {
              xmonad = final.xmonad_0_18_0;
              weiss-xmonad = final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc966";
              };
            })
            devshell.overlays.default
          ];
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          flake = pkgs.weiss-xmonad.flake { };
          shellWithToml =
            tomls:
            pkgs.weiss-xmonad.shellFor {
              exactDeps = false;
              withHoogle = true;
              inputsFrom = [
                (pkgs.devshell.fromTOML ./devshell.toml)
                # (pkgs.devshell.mkShell {
                #   name = "weiss-xmonad";
                #   packages = with pkgs; [ ];
                # })
              ];
              tools = {
                cabal = "latest";
                fourmolu = "latest";
                hlint = "latest";
                cabal-fmt = "latest";
                haskell-language-server = "latest";
              };
            };
        in
        flake
        // {
          packages = {
            default = flake.packages."weiss-xmonad:exe:weiss-xmonad-exe";
          };
          devShells = {
            default = shellWithToml [ ];
          };
        }
      );

}
