# Python TDD FizzBuzz

テスト駆動開発から始めるPython入門 〜ソフトウェア開発の三種の神器を準備する〜

## プロジェクト概要

このプロジェクトは、Rubyで実装された「テスト駆動開発から始めるRuby入門」をPythonに移植したものです。ソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）を学習するためのサンプルプロジェクトです。

## ソフトウェア開発の三種の神器

1. **バージョン管理**: Git
2. **テスティング**: pytest
3. **自動化**: 
   - 静的コード解析: pylint, flake8
   - コードフォーマッタ: black
   - 型チェック: mypy
   - コードカバレッジ: coverage.py
   - タスクランナー: make
   - 自動テスト実行: pytest-watch

## 必要な環境

- Python 3.8以上
- pip

## セットアップ

1. 開発依存関係のインストール:
```bash
make install-dev
```

2. 開発環境のセットアップ:
```bash
make setup
```

## 使用方法

### テスト実行

```bash
# 通常のテスト実行
make test

# 自動テスト実行（ファイル変更監視）
make test-watch

# カバレッジ付きテスト実行
make coverage
```

### コード品質チェック

```bash
# すべてのチェックを実行
make all

# 静的解析
make lint

# コードフォーマット
make format

# 型チェック
make type-check
```

### その他のタスク

```bash
# ヘルプの表示
make help

# 生成ファイルのクリーンアップ
make clean
```

## プロジェクト構成

```
├── src/                    # ソースコード
│   ├── __init__.py
│   └── fizzbuzz.py        # FizzBuzz実装
├── tests/                 # テストコード
│   ├── __init__.py
│   └── test_fizzbuzz.py   # FizzBuzzテスト
├── requirements.txt       # 本番依存関係
├── requirements-dev.txt   # 開発依存関係
├── pyproject.toml        # プロジェクト設定
├── .pylintrc             # Pylint設定
├── Makefile              # タスクランナー
└── README.md             # このファイル
```

## FizzBuzz仕様

- 数値を受け取り、以下のルールに従って文字列を返す
  - 3の倍数の場合: "Fizz"
  - 5の倍数の場合: "Buzz"  
  - 3と5の両方の倍数の場合: "FizzBuzz"
  - それ以外の場合: 数値の文字列表現

## 学習内容

このプロジェクトでは以下を学習できます：

1. **テスト駆動開発（TDD）**
   - Red-Green-Refactorサイクル
   - テストファーストの開発手法

2. **静的コード解析**
   - Pylint: コード品質の分析
   - Flake8: PEP8準拠チェック
   - MyPy: 型チェック

3. **コードフォーマッティング**
   - Black: 自動コードフォーマット

4. **テスト自動化**
   - pytest: テストフレームワーク
   - coverage.py: コードカバレッジ測定
   - pytest-watch: ファイル変更監視

5. **タスク自動化**
   - Makefile: タスクランナー
   - 開発ワークフローの自動化

## Pythonの配列・イテレータ操作学習

Ruby記事の学習テストをPython版に移植し、以下の操作を学習できます：

- リスト内包表記
- filter/map操作
- sorted関数
- itertools モジュール
- functools.reduce
- 正規表現パターンマッチング

## 開発フロー

1. テストを書く（Red）
2. 最小限の実装でテストを通す（Green）  
3. コードを改善する（Refactor）
4. 自動化ツールでコード品質を維持

このサイクルを繰り返すことで、高品質なコードを継続的に開発できます。
