{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      haskellNix,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            project = final.haskell-nix.project' {
              src = ./.;
              name = "xmonad-launch";
              compiler-nix-name = "ghc984";
              shell = {
                tools = {
                  fourmolu = "latest";
                  haskell-language-server = "latest";
                  hlint = "latest";
                  stylish-haskell = "latest";
                };
                buildInputs = with prev; [
                  (writeScriptBin "haskell-language-server-wrapper" ''
                    #!${stdenv.shell}
                    exec haskell-language-server "$@"
                  '')

                  prev.gtk3
                ];
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.project.flake { };
      in
      flake
      // {
        packages = flake.packages // {
          xmonad-launch = flake.packages."xmonad-launch:exe:xmonad-launch";
          xmobar-launch = flake.packages."xmonad-launch:exe:xmobar-launch";
        };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://cache.iog.io"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    allow-import-from-derivation = "true";
  };
}
