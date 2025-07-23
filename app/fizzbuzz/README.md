# FizzBuzz - Erlang TDD Example

テスト駆動開発から始めるErlang入門のサンプルプロジェクトです。

## ソフトウェア開発の三種の神器

このプロジェクトではソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）を活用した開発環境を提供します。

### 1. バージョン管理 (Git)
```bash
# 変更をコミット
git add .
git commit -m "feat: 新機能の追加"
```

### 2. テスティング (EUnit)
```bash
# テスト実行
make test

# カバレッジ付きテスト実行
make cover

# または直接rebar3を使用
rebar3 eunit
rebar3 eunit --cover
```

### 3. 自動化 (rebar3 + Make + 監視スクリプト)

#### 基本的なビルドタスク
```bash
# プロジェクトをコンパイル
make compile

# テスト実行
make test

# 静的解析実行
make dialyzer

# リンター実行  
make lint

# 全チェック実行
make check

# クリーンアップ
make clean

# インタラクティブシェル起動
make shell
```

#### 自動監視
```bash
# ファイル変更を監視して自動テスト実行
./watch.sh
```

## 開発環境セットアップ

### 必要なソフトウェア
- Erlang/OTP 26.2.1以上
- rebar3
- make

### プロジェクトセットアップ
```bash
# 依存関係の取得とコンパイル
rebar3 compile

# テスト実行
rebar3 eunit

# 開発用監視開始
./watch.sh
```

## プロジェクト構造

```
fizzbuzz/
├── rebar.config          # rebar3設定ファイル
├── rebar.lock            # 依存関係ロックファイル
├── elvis.config          # Elvis（リンター）設定
├── Makefile              # タスクランナー定義
├── watch.sh              # ファイル監視スクリプト
├── src/                  # ソースコード
│   ├── fizzbuzz.app.src  # アプリケーション定義
│   ├── fizzbuzz.erl      # メインモジュール
│   ├── fizzbuzz_app.erl  # アプリケーションコールバック
│   └── fizzbuzz_sup.erl  # スーパーバイザー
└── test/                 # テストコード
    └── fizzbuzz_tests.erl # EUnitテスト
```

## 機能

### FizzBuzz変換
数値を文字列に変換し、3の倍数は"Fizz"、5の倍数は"Buzz"、両方の倍数は"FizzBuzz"に変換します。

```erlang
1> fizzbuzz:convert(1).
"1"
2> fizzbuzz:convert(3).
"Fizz"
3> fizzbuzz:convert(5).
"Buzz"
4> fizzbuzz:convert(15).
"FizzBuzz"
```

### 1から100までのFizzBuzz生成
```erlang
1> List = fizzbuzz:fizzbuzz_list().
["1","2","Fizz","4","Buzz",...,"Buzz"]
```

### プリント機能
```erlang
1> fizzbuzz:print_fizzbuzz().
1
2
Fizz
4
Buzz
...
ok
```

## 開発ワークフロー

1. **コード変更**
2. **自動テスト実行** (watch.shを使用している場合)
3. **静的解析** (`make dialyzer`)
4. **リンター** (`make lint`)
5. **コミット** (`git commit`)

## 学習のポイント

- **テスト駆動開発**: テストファースト → 実装 → リファクタリング
- **継続的な品質向上**: 静的解析とリンターによるコード品質維持
- **自動化**: 繰り返し作業の自動化による効率向上

このプロジェクトを通じて、Erlangでのテスト駆動開発と現代的な開発ツールチェーンの使い方を学ぶことができます。
