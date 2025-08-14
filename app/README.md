# テスト駆動開発から始めるClojureScript入門

## プロジェクト概要

このプロジェクトは「テスト駆動開発から始めるClojure入門」のClojureScript版実装です。
shadow-cljsとGulpを使用してFizzBuzzアプリケーションを構築し、ソフトウェア開発の三種の神器（バージョン管理・テスティング・自動化）を実践します。

## 技術スタック

- **ClojureScript**: メインの開発言語
- **shadow-cljs**: ClojureScriptビルドツール
- **npm**: 依存関係管理
- **Gulp**: タスクランナー
- **cljs.test**: テストフレームワーク
- **clj-kondo**: 静的コード解析ツール
- **cljfmt**: コードフォーマッター
- **Clojure CLI**: 依存関係管理とツール実行

## ソフトウェア開発の三種の神器

### 1. バージョン管理
- Gitによるソースコード管理
- Angularルールに基づくコミットメッセージ

### 2. テスティング
- cljs.testによる自動テスト
- テスト駆動開発（TDD）の実践

### 3. 自動化
- shadow-cljsによるビルド自動化
- Gulpによるタスクランナー
- npmによる依存関係管理
- clj-kondoによる静的コード解析
- cljfmtによるコードフォーマット自動化

## セットアップ

### 前提条件
- Node.js (v16以上推奨)
- npm
- Java (shadow-cljsの実行に必要)
- Clojure CLI (ツールチェーンに必要)

### インストール

```bash
# 依存関係のインストール
npm install

# または Gulpタスクを使用
npx gulp setup
```

## 利用可能なタスク

Gulpで定義されたタスクは以下の通りです：

| タスク | コマンド | 説明 |
|--------|----------|------|
| ヘルプ | `npx gulp help` | 利用可能なタスクを表示 |
| セットアップ | `npx gulp setup` | 依存関係のインストール |
| テスト | `npx gulp test` | 自動テストを実行 |
| ビルド | `npx gulp build` | アプリケーションをビルド |
| Watch | `npx gulp watch` | 開発モードでファイルを監視 |
| リリース | `npx gulp release` | リリースビルドを実行 |
| サーバー | `npx gulp server` | shadow-cljsサーバーを起動 |
| 開発環境 | `npx gulp dev` | サーバー + watchを同時実行 |
| クリーンアップ | `npx gulp clean` | 生成ファイルを削除 |
| 品質チェック | `npx gulp check` | 全ての品質チェックを実行 |
| 静的解析 | `npx gulp lint` | 静的コード解析を実行 |
| フォーマット | `npx gulp format` | コードフォーマットを確認 |
| フォーマット修正 | `npx gulp format-fix` | コードフォーマットを自動修正 |
| カバレッジ | `npx gulp coverage` | テストカバレッジを測定 |

### npmスクリプトとの連携

```bash
# 開発環境の起動
npm run dev

# 品質チェック
npm run check

# テスト実行
npm test

# ビルド
npm run build

# Clojureツールチェーン
npm run lint          # clj-kondoによる静的解析
npm run format        # cljfmtによるフォーマットチェック
npm run format-fix    # cljfmtによるフォーマット自動修正
npm run coverage      # カバレッジ測定（拡張版）
npm run outdated      # 依存関係の更新チェック
```

## 開発ワークフロー

### 1. 開発環境の起動

```bash
# shadow-cljsサーバーとwatchを同時起動
npx gulp dev
```

ブラウザで http://localhost:8080 にアクセスしてアプリケーションを確認できます。
shadow-cljsの管理画面は http://localhost:9630 で確認できます。

### 2. テスト駆動開発の実践

```bash
# テストを実行
npx gulp test

# ファイル変更を監視してテストを自動実行
npx gulp watch
```

### 3. 品質チェック

```bash
# 全ての品質チェックを実行
npx gulp check
```

## Clojureツールチェーン

このプロジェクトではClojure CLI（deps.edn）を使用してClojure固有のツールを管理しています。

### 利用可能なツール

| ツール | エイリアス | 説明 |
|--------|------------|------|
| **clj-kondo** | `:lint` | 静的コード解析ツール |
| **cljfmt** | `:format-check` | コードフォーマットチェック |
| **cljfmt** | `:format-fix` | コードフォーマット自動修正 |
| **antq** | `:outdated` | 依存関係の更新チェック |

### 設定ファイル

- **`.clj-kondo/config.edn`**: clj-kondoの設定（ClojureScript対応）
- **`.cljfmt.edn`**: cljfmtのフォーマット設定
- **`deps.edn`**: Clojure CLI依存関係とエイリアス定義

### 直接実行

Clojure CLIを直接使用することも可能です：

```bash
# 静的解析
clojure -M:lint

# フォーマットチェック
clojure -M:format-check src/ test/

# フォーマット自動修正
clojure -M:format-fix src/ test/

# 依存関係更新チェック
clojure -M:outdated
```

## プロジェクト構成

```
app/
├── gulpfile.js          # Gulpタスクランナー設定
├── package.json         # npm設定
├── deps.edn             # Clojure CLI設定
├── shadow-cljs.edn      # shadow-cljs設定
├── .clj-kondo/
│   └── config.edn       # clj-kondo設定
├── .cljfmt.edn          # cljfmt設定
├── README.md            # このファイル
├── public/
│   ├── index.html       # HTMLエントリーポイント
│   └── js/              # ビルド済みJavaScript出力先
├── src/
│   └── fizzbuzz/
│       └── core.cljs    # FizzBuzzメイン実装
├── test/
│   └── fizzbuzz/
│       └── core_test.cljs # FizzBuzzテスト
└── out/                 # テスト出力先
```

## テスト駆動開発の実践

このプロジェクトでは以下のTDDサイクルに従って開発を進めます：

1. **Red**: 失敗するテストを書く
2. **Green**: テストをパスする最小限のコードを書く
3. **Refactor**: コードをリファクタリングして品質を向上させる

### テスト例

```clojure
(deftest fizzbuzz-test
  (testing "数を文字列にして返す"
    (is (= "1" (fizzbuzz/fizzbuzz 1))))
  
  (testing "3の倍数のときは「Fizz」と返す"
    (is (= "Fizz" (fizzbuzz/fizzbuzz 3))))
  
  (testing "5の倍数のときは「Buzz」と返す"
    (is (= "Buzz" (fizzbuzz/fizzbuzz 5))))
  
  (testing "3と5両方の倍数の場合には「FizzBuzz」と返す"
    (is (= "FizzBuzz" (fizzbuzz/fizzbuzz 15)))))
```

## コミットメッセージ規約

Angularルールに従ったコミットメッセージを使用します：

```
<タイプ>(<スコープ>): <タイトル>

例:
feat: FizzBuzz基本機能の実装
fix: バグ修正
docs: ドキュメント更新
test: テスト追加
refactor: リファクタリング
```

## 学習リソース

- [ClojureScript公式ドキュメント](https://clojurescript.org/)
- [shadow-cljs公式ガイド](https://shadow-cljs.github.io/docs/UsersGuide.html)
- [テスト駆動開発 - Kent Beck](https://www.amazon.co.jp/dp/4274217884)
- [リーダブルコード](https://www.amazon.co.jp/dp/4873115655)

## ライセンス

MIT License
