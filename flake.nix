{
  inputs = {
    nixpkgs.follows = "nixpkgs-2511";
    nixpkgs-2511.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gtk2hs = {
      url = "github:ncaq/gtk2hs/260805611e54e60383c1d98c5c0a29ad9c2efe7f";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];

      imports = [ inputs.treefmt-nix.flakeModule ];

      perSystem =
        {
          pkgs,
          config,
          lib,
          ...
        }:
        let
          haskellPackages = pkgs.haskell.packages.ghc9103.override {
            overrides = hself: hsuper: {
              # gtk2hs packages from inputs.gtk2hs
              # callCabal2nixの第3引数でpkg-config依存のHaskellパッケージ名との衝突を解決する。
              # cabal2nixはpkgconfig-dependsもHaskellパッケージセットから探すため、
              # glib, cairo等のシステムライブラリ名がHaskellパッケージ名と衝突して無限再帰になる。
              gtk2hs-buildtools = hself.callCabal2nix "gtk2hs-buildtools" (inputs.gtk2hs + "/tools") { };
              # cabal2nixはpkgconfig-dependsの名前をそのまま関数引数にするため、
              # システムライブラリ名がHaskellパッケージ名と衝突する。
              # 第3引数で明示的にシステムパッケージを渡して衝突を解決する。
              glib = hself.callCabal2nix "glib" (inputs.gtk2hs + "/glib") {
                inherit (pkgs) glib;
              };
              cairo = hself.callCabal2nix "cairo" (inputs.gtk2hs + "/cairo") {
                inherit (pkgs) cairo;
              };
              gio = hself.callCabal2nix "gio" (inputs.gtk2hs + "/gio") {
                system-glib = pkgs.glib;
              };
              pango = hself.callCabal2nix "pango" (inputs.gtk2hs + "/pango") {
                inherit (pkgs) pango;
              };
              # cabal2nixはgtk3パッケージとして生成する(gtk/ディレクトリだがcabal名がgtk3)
              gtk3 = hself.callCabal2nix "gtk3" (inputs.gtk2hs + "/gtk") {
                inherit (pkgs) gtk3;
              };

              # xmobar with flags
              xmobar = pkgs.haskell.lib.compose.enableCabalFlag "with_datezone" (
                pkgs.haskell.lib.compose.enableCabalFlag "with_threaded" hsuper.xmobar
              );

              xmonad-launch = hself.callCabal2nix "xmonad-launch" inputs.self { };
            };
          };

          xmonad-helper-bin = pkgs.runCommand "xmonad-helper-bin" { } ''
            mkdir -p $out/bin
            cp ${./bin}/* $out/bin/
            chmod +x $out/bin/*
          '';

          xmonad-launch-full = pkgs.symlinkJoin {
            name = "xmonad-launch-full";
            paths = with pkgs; [
              birdtray
              copyq
              haskellPackages.xmonad-launch
              imagemagick
              networkmanagerapplet
              oxipng
              snixembed
              trayer
              xmonad-helper-bin
              xorg.xinput
            ];
          };
        in
        {
          packages = {
            default = xmonad-launch-full;
          };

          checks = {
            xmonad-launch = pkgs.haskell.lib.compose.appendConfigureFlags [
              "--ghc-options=-Werror"
            ] haskellPackages.xmonad-launch;
            formatting = config.treefmt.build.check inputs.self;
          };

          treefmt = {
            # yamlfmtはprettierと競合します。
            projectRootFile = "flake.nix";
            programs = {
              actionlint.enable = true;
              deadnix.enable = true;
              fourmolu.enable = true;
              hlint.enable = true;
              nixfmt.enable = true;
              prettier.enable = true;
              shellcheck.enable = true;
              shfmt.enable = true;
              statix.enable = true;
            };
            settings.formatter = {
              # cabal-gildのモジュール自動発見機能に対応するため、
              # Haskellソースファイルの変更も検知してcabal-gildを実行します。
              # treefmt-nixの上流では変更されたファイルだけを修正したいと言われてマージされていませんが、
              # ローカルで使う分には問題ありません。
              # [fix(cabal): cabal-fmt and cabal-gild discover module by ncaq · Pull Request #384 · numtide/treefmt-nix](https://github.com/numtide/treefmt-nix/pull/384)
              cabal-gild = {
                command = lib.getExe (
                  pkgs.writeShellApplication {
                    name = "cabal-gild-wrapper";
                    runtimeInputs = with pkgs; [
                      git
                      haskellPackages.cabal-gild
                      parallel
                    ];
                    text = ''
                      git ls-files -z "*.cabal" | parallel --null "cabal-gild --io {}"
                    '';
                  }
                );
                includes = [
                  "*.cabal"
                  # Haskellソースファイルの変更を検知するために含める
                  "*.hs"
                  "*.lhs"
                  "*.hsc"
                  "*.chs"
                  "*.hsig"
                  "*.lhsig"
                ];
              };
              editorconfig-checker = {
                command = lib.getExe (
                  pkgs.writeShellApplication {
                    name = "editorconfig-checker-wrapper";
                    runtimeInputs = [ pkgs.editorconfig-checker ];
                    text = ''
                      editorconfig-checker -config .editorconfig-checker.json "$@"
                    '';
                  }
                );
                includes = [ "*" ];
                excludes = [
                  ".direnv/*"
                  ".git/*"
                  "dist-newstyle/*"
                  "result*"
                ];
              };
            };
          };

          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.xmonad-launch ];
            nativeBuildInputs = with pkgs; [
              cabal-install
              fourmolu
              gtk3
              haskell-language-server
              haskellPackages.cabal-gild
              hlint
              nil
              nixfmt
              yamllint

              (pkgs.writeScriptBin "haskell-language-server-wrapper" ''
                #!${pkgs.stdenv.shell}
                exec haskell-language-server "$@"
              '')
            ];
          };
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://ncaq-xmonad.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "ncaq-xmonad.cachix.org-1:ngWi7GYMyNG5XYhjqRnPphQdViZdZcJ5b+IZ1FD3ebg="
    ];
    allow-import-from-derivation = true;
  };
}
