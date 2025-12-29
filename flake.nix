{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      haskellNix,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        ghc-version = "ghc9103"; # GHC 9.10.3
        overlays = [
          haskellNix.overlay
          (
            final: _prev:
            let
              # haskell.nixのtoolsで参照されるhaskell-language-server。
              tool-haskell-language-server =
                final.haskell-nix.tool ghc-version "haskell-language-server"
                  "latest";
            in
            {
              # nixpkgsで普通にインストールされるfourmoluはhaskell-language-serverのものと違うので上書きして合わせる。
              inherit (tool-haskell-language-server.project.hsPkgs.fourmolu.components.exes) fourmolu;
            }
          )
          (final: prev: {
            project = final.haskell-nix.project' {
              src = ./.;
              name = "xmonad-launch";
              compiler-nix-name = ghc-version;
              modules = [
                # `nix flake check`レベルではcabalの警告をエラーとして扱います。
                # ライブラリの問題ない範囲の不一致とか考えるとcabalの警告はエラーにしないべきですが、
                # CIでは通したくないので警告も含めてエラーにします。
                # CI専用に環境を分離するのも手ですが、
                # 元々`nix flake check`では最適化を無効にするのが面倒なので時間がかかるため、
                # あまり反復的に実行しません。
                # 反復的に実行してテストの結果とかを確認するのには普通は`cabal test`のような言語固有のコマンドを使います。
                # `cabal test`の方が実行するテストをフィルタリングとかも簡単に出来ますし。
                # そのことを考えると本番を考えてエラーにしても構わないでしょう。
                # flake参照された時にnixpkgsをfollowすると問題ない警告をエラーにしてしまうかもしれないのが懸念点ですが、
                # 少なくとも現在は考慮する必要はないでしょう。
                # 注意点として、srcやtestはビルドされますが、executableであるappはビルドされません。
                # 「appはエントリーポイントとしてのみ使う」習慣を守っていれば問題にはならないです。
                (
                  { lib, config, ... }:
                  {
                    # パッケージたちをハードコーディングすると変更忘れが発生するので`config.package-keys`で取得。
                    options.packages = lib.genAttrs config.package-keys (
                      _name:
                      lib.mkOption {
                        type = lib.types.submodule (
                          { config, lib, ... }:
                          # `cabal.project`に`source-repository-package`などで書かれていたりする、
                          # 外部パッケージは変更したくないので、
                          # `isProject`でフィルタリングしています。
                          lib.mkIf config.package.isProject {
                            # この書き方で上書きではなく追加として扱われる。
                            ghcOptions = [ "-Werror" ];
                          }
                        );
                      }
                    );
                  }
                )
              ];
              shell = {
                tools = {
                  cabal = "latest";
                  cabal-gild = "latest"; # treefmtで管理されているがvscodeのHaskell拡張向けに使えるようにしておく
                  haskell-language-server = "latest";
                  hlint = "latest";
                  implicit-hie = "latest";
                };
                buildInputs = with prev; [
                  fourmolu
                  nil
                  nixfmt-rfc-style
                  parallel
                  prev.gtk3
                  yamllint

                  (writeScriptBin "haskell-language-server-wrapper" ''
                    #!${stdenv.shell}
                    exec haskell-language-server "$@"
                  '')
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
        treefmtEval = treefmt-nix.lib.evalModule pkgs (_: {
          # actionlintはセルフホストランナーの設定ファイルを正常に読み込まなかった。
          # yamlfmtはprettierと競合する。
          projectRootFile = "flake.nix";
          programs = {
            cabal-gild.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            nixfmt.enable = true;
            prettier.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
            statix.enable = true;

            fourmolu = {
              enable = true;
              package = pkgs.fourmolu;
            };
          };
        });
        xmonad-launch = flake.packages."xmonad-launch:exe:xmonad-launch";
        xmobar-launch = flake.packages."xmonad-launch:exe:xmobar-launch";
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
            networkmanagerapplet
            trayer
            xmobar-launch
            xmonad-helper-bin
            xmonad-launch
            xorg.xinput
          ];
        };
      in
      # haskell.nixのproject.flakeはciJobsとhydraJobsを生成するが、
      # ciJobsは非標準のoutputであり警告を誘発し、
      # hydraJobsもGitHub Actionsを使うため不要なので除外。
      builtins.removeAttrs flake [
        "ciJobs"
        "hydraJobs"
      ]
      // {
        checks =
          flake.packages # テストがないパッケージもビルドしてエラーを検出する。
          // flake.checks
          // {
            formatting = treefmtEval.config.build.check self;
          };
        formatter = treefmtEval.config.build.wrapper;
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
      "https://ncaq-xmonad.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "ncaq-xmonad.cachix.org-1:ngWi7GYMyNG5XYhjqRnPphQdViZdZcJ5b+IZ1FD3ebg="
    ];
    allow-import-from-derivation = true;
  };
}
