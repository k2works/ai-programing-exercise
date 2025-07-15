# テスト駆動開発から始めるPython入門

このプロジェクトは「テスト駆動開発から始めるRuby入門2」の内容をPythonで実装したものです。ソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）を活用して、良いコードを書くための環境を構築しています。

## プロジェクト概要

FizzBuzzプログラムを通してテスト駆動開発（TDD）の基本概念を学び、同時にPythonの配列操作や反復処理についても学習できるサンプルプロジェクトです。

## 技術スタック

- **Python**: 3.10以上
- **パッケージマネージャー**: [uv](https://github.com/astral-sh/uv)
- **テストフレームワーク**: [pytest](https://pytest.org/)
- **コードカバレッジ**: [pytest-cov](https://pytest-cov.readthedocs.io/)
- **リンター・フォーマッター**: [Ruff](https://github.com/astral-sh/ruff)
- **型チェック**: [mypy](https://mypy.readthedocs.io/)
- **タスクランナー**: [tox](https://tox.wiki/)

## セットアップ

### 前提条件

- Python 3.10以上
- [uv](https://github.com/astral-sh/uv)がインストールされていること

### インストール

```bash
# 依存関係のインストール
uv install

# 開発依存関係のインストール
uv add --dev pytest pytest-cov ruff mypy tox
```

## 使用方法

### テストの実行

```bash
# 基本的なテスト実行
uv run pytest

# 詳細なテスト結果とカバレッジを表示
uv run pytest -v --cov=lib --cov-report=term-missing

# HTMLカバレッジレポートの生成
uv run pytest --cov=lib --cov-report=html
```

### コード品質チェック

```bash
# リンターの実行
uv run ruff check .

# フォーマッターの実行
uv run ruff format .

# 型チェックの実行
uv run mypy lib test
```

### タスクランナー（tox）

```bash
# すべてのタスクを実行（推奨）
uv run tox

# 個別タスクの実行
uv run tox -e test        # テストのみ
uv run tox -e lint        # リントのみ
uv run tox -e type        # 型チェックのみ
uv run tox -e coverage    # カバレッジレポートのみ
uv run tox -e format      # フォーマットのみ
```

## プロジェクト構造

```
app/
├── lib/                    # ライブラリコード
│   ├── __init__.py
│   └── fizz_buzz.py       # FizzBuzzクラスの実装
├── test/                   # テストコード
│   ├── __init__.py
│   └── test_fizz_buzz.py  # FizzBuzzのテストと学習テスト
├── .gitignore             # Git無視ファイル
├── .ruff.toml             # Ruff設定
├── pyproject.toml         # プロジェクト設定
├── tox.ini                # tox設定
├── README.md              # このファイル
└── uv.lock                # 依存関係のロックファイル
```

## FizzBuzzとは

1から100までの数字を出力するプログラムですが、以下のルールがあります：

- 3の倍数の場合は「Fizz」を出力
- 5の倍数の場合は「Buzz」を出力
- 3と5の両方の倍数（15の倍数）の場合は「FizzBuzz」を出力
- それ以外の場合は数字をそのまま出力

## 学習内容

このプロジェクトでは以下を学習できます：

### 1. テスト駆動開発（TDD）
- Red-Green-Refactorサイクル
- テストファーストの開発手法
- リファクタリングの重要性

### 2. Pythonの配列・反復処理
- リスト内包表記
- `filter()`, `map()`, `reduce()`関数
- `sorted()`, `next()`などの組み込み関数
- 条件に基づく要素の抽出・変換

### 3. ソフトウェア開発の三種の神器
- **バージョン管理**: Git
- **テスティング**: pytest + カバレッジ
- **自動化**: tox + CI/CD

### 4. コード品質管理
- 静的コード解析（Ruff）
- 型チェック（mypy）
- コードフォーマット
- テストカバレッジ

## 開発ワークフロー

1. テストを書く（Red）
2. テストをパスする最小限のコードを書く（Green）
3. コードをリファクタリングする（Refactor）
4. `uv run tox`で品質チェック
5. コミット

## コミットメッセージ規約

[Angularルール](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#type)に従ったコミットメッセージを使用：

- `feat:` 新機能
- `fix:` バグ修正
- `docs:` ドキュメント変更のみ
- `style:` コードに影響を与えない変更
- `refactor:` 機能追加でもバグ修正でもないコード変更
- `test:` テストの追加・修正
- `chore:` ビルドプロセスや補助ツールの変更

## ライセンス

このプロジェクトは教育目的で作成されています。