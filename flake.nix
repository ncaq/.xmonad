{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
        xmonad-launch = flake.packages."xmonad-launch:exe:xmonad-launch";
        xmobar-launch = flake.packages."xmonad-launch:exe:xmobar-launch";
        xmonad-helper-bin = pkgs.runCommandNoCC "xmonad-helper-bin" { } ''
          mkdir -p $out/bin
          cp ${./bin}/* $out/bin/
          chmod +x $out/bin/*
        '';
        xmonad-launch-full = pkgs.symlinkJoin {
          name = "xmonad-launch-full";
          paths = with pkgs; [
            birdtray
            copyq
            networkmanagerapplet
            trayer
            xmobar-launch
            xmonad-helper-bin
            xmonad-launch
            xorg.xinput
          ];
        };
      in
      flake
      // {
        packages = flake.packages // {
          default = xmonad-launch-full;
          xmobar-launch = flake.packages."xmonad-launch:exe:xmobar-launch";
          xmonad-launch = flake.packages."xmonad-launch:exe:xmonad-launch";
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
    allow-import-from-derivation = true;
  };
}
