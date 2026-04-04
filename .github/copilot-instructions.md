# 出力設定

## 言語

AIは人間に話すときは日本語を使ってください。

しかし既存のコードのコメントなどが日本語ではない場合は、
コメント等は既存の言語に合わせてください。

## 記号

ASCIIに対応する全角形(Fullwidth Forms)は使用禁止。

具体的には以下のような文字:

- 全角括弧 `（）` → 半角 `()`
- 全角コロン `：` → 半角 `:`
- 全角カンマ `，` → 半角 `,`
- 全角数字 `０-９` → 半角 `0-9`

# Nix言語

## 命名規則

[nixpkgsの公式コーディング規約](https://github.com/NixOS/nixpkgs/blob/master/pkgs/README.md)

### ファイル名・ディレクトリ名

kebab-caseを使用します。

例: `all-packages.nix`, `claude-code.nix`

### 変数名・属性名

| 種類                   | スタイル       | 例                                                         |
| ---------------------- | -------------- | ---------------------------------------------------------- |
| 純粋な変数・設定値     | lowerCamelCase | `keyConfig`, `identityKey`, `baseProfile`                  |
| パッケージ・derivation | kebab-case     | `github-mcp-server-wrapper`, `trayscale-autostart-desktop` |

単純な変数はlowerCamelCaseを使用します。

パッケージやプログラムを示す変数は、
pnameと同様にkebab-caseを使用します。
2012年以降、
Nix言語では識別子にハイフンを使用できます。

### NixOSオプション

原則camelCaseを使用します。

例: `services.nginx.enableReload`, `prompt.chatAssistant`

例外:

- パッケージ名を参照する場合はkebab-case: `services.nix-serve`
- `nix.settings`など外部設定ファイルをマッピングするオプションは、その設定ファイルの命名規則に従う(nix.confはkebab-case)

## `writeShellApplication`

デフォルトの安全性と分かりやすさの点で、
基本的に`writeShellScript`などよりも、
`writeShellApplication`を優先的に使用します。

# 重要コマンド

## `nix fmt`

[treefmt-nix](https://github.com/numtide/treefmt-nix)が対応しているファイルは以下のコマンドでフォーマット出来ます。

```console
nix fmt
```

Stopフックで`nix fmt`が自動実行されます。
ファイルの差分が出ることがあります。

フォーマットという名前ですが、
リンターも登録されていて動作します。

## `nix flake check`

`nix flake check`コマンドでプロジェクト全体のチェックが行えます。
フォーマットやリントやテストなどがまとめて実行されます。

クリーンな環境で実行されます。
実行する必要のないものはキャッシュされてスキップされるため、
下手に使い分けるよりも`nix flake check`を使うのが推奨されます。

nixの意図的な設計として、
Gitにトラッキングされていないファイルはnixに認識されません。

テストを実行する前に以下のコマンドで全てのファイルをトラッキングさせてください。

```console
git add --intent-to-add .
```

その後自動修正出来る範囲のエラーを避けるために、
`nix fmt`を実行してください。

そしてオプションで実行してください。

```console
nix flake check
```

テストにnixを介さずに実行するのは推奨されません。
必要な準備が自動で実行されないことがあるためです。

# リポジトリ構成

Codex向けの`AGENTS.md`とClaude Code向けの`CLAUDE.md`は以下のように`.github/copilot-instructions.md`のシンボリックリンクになっています。

```console
AGENTS.md -> .github/copilot-instructions.md
CLAUDE.md -> .github/copilot-instructions.md
```

これにより各種LLM向けのドキュメントを一元管理しています。
