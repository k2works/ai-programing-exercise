# fizz-buzz-clj

テスト駆動開発から始めるClojure入門 ~ソフトウェア開発の三種の神器を準備する~

## 概要

このプロジェクトは、RubyのTDD記事をClojureで実装したものです。ソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）を実践しながら、FizzBuzzアルゴリズムをテスト駆動開発で実装しています。

## 技術スタック

- **言語**: Clojure 1.11.1
- **ビルドツール**: Leiningen 2.11.2
- **依存関係管理**: deps.edn
- **テストフレームワーク**: clojure.test + test-runner
- **静的コード解析**: clj-kondo
- **コードフォーマッター**: cljfmt
- **タスクランナー**: Make

## セットアップ

```bash
# 依存関係の確認
make setup

# プロジェクトのクローン後、以下のコマンドでセットアップ完了
make help
```

## 使用方法

### アプリケーション実行

```bash
# FizzBuzzアプリケーションを実行
make run

# または直接実行
clojure -M -m fizz-buzz-clj.core
```

### テスト実行

```bash
# テストを実行
make test

# または直接実行
clojure -M:test
```

### 品質チェック

```bash
# 全ての品質チェックを実行
make check

# 個別に実行
make lint      # 静的コード解析
make format    # コードフォーマットチェック
make test      # テスト実行
```

### 自動化

```bash
# コードフォーマット自動修正
make format-fix

# ファイル監視と自動テスト実行（要: entr）
make auto
```

## ソフトウェア開発の三種の神器

1. **バージョン管理**: Git
2. **テスティング**: clojure.test + test-runner
3. **自動化**: Make + clj-kondo + cljfmt + 自動テスト

## 学習用テスト

このプロジェクトには、Clojureの配列操作を学ぶための学習用テストが含まれています：

- `map`: 新しい要素の配列を返す
- `filter`: 特定の条件を満たす要素だけを配列に入れて返す
- `remove`: 特定の条件を満たさない要素だけを配列に入れて返す
- `sort-by`: 指定した評価式で並び変えた配列を返す
- `take-while` / `drop-while`: 条件による要素の取得
- `reduce`: 畳み込み演算

## コミットメッセージ規約

Angularルールに従ったコミットメッセージを推奨：

- `feat:` 新しい機能
- `fix:` バグ修正
- `docs:` ドキュメント変更
- `style:` コードフォーマット等
- `refactor:` リファクタリング
- `test:` テスト追加・修正
- `chore:` ビルドプロセス・補助ツール関連

## ライセンス

Copyright © 2025 k2works

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
