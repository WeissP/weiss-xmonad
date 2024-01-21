{
  description = "weiss-xmonad";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              weiss-xmonad = hfinal.callCabal2nix "weiss-xmonad" ./. { };
            };
        };
        weiss-xmonad = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.weiss-xmonad;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = weiss-xmonad-shell;
            weiss-xmonad-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.weiss-xmonad ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = weiss-xmonad;
            weiss-xmonad = pkgs.weiss-xmonad;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
