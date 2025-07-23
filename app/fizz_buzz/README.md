# FizzBuzz

テスト駆動開発（TDD）で実装されたElixir版FizzBuzzアプリケーションです。

FizzBuzz問題を解くと同時に、Elixirの基本的な機能とソフトウェア開発の三種の神器（バージョン管理、テスト、自動化）を学習できます。

## 機能

- **数値からFizzBuzz判定**: 個別の数値に対するFizzBuzz判定
- **範囲処理**: 1からN までのFizzBuzz配列生成
- **コンソール出力**: 結果をコンソールに出力
- **関数型アプローチ**: パターンマッチングとパイプライン演算子を活用

## 使用例

```elixir
# Elixir対話シェルを起動
iex -S mix

# 個別の数値を判定
FizzBuzz.generate(1)    # "1"
FizzBuzz.generate(3)    # "Fizz"
FizzBuzz.generate(5)    # "Buzz"
FizzBuzz.generate(15)   # "FizzBuzz"

# 1から100までの配列を生成
FizzBuzz.list(100)      # ["1", "2", "Fizz", "4", "Buzz", ...]

# コンソールに出力（デフォルト: 1-100）
FizzBuzz.print()        # 1から100まで出力
FizzBuzz.print(20)      # 1から20まで出力
```

## 開発環境

このプロジェクトでは以下の開発ツールを使用しています：

### ソフトウェア開発の三種の神器

1. **バージョン管理**: Git
2. **テスティング**: ExUnit + ExCoveralls
3. **自動化**: Mix tasks + 静的解析 + フォーマッタ

### 使用ツール

- **静的コード解析**: Credo
- **コードフォーマッタ**: mix format
- **コードカバレッジ**: ExCoveralls
- **ファイル監視**: mix_test_watch

## セットアップ

```bash
# 依存関係をインストール
mix deps.get

# テストを実行
mix test

# コードカバレッジを確認
mix coveralls
```

## 開発コマンド

### 基本コマンド
```bash
mix test               # テスト実行
mix format             # コード整形
mix credo              # 静的解析
mix coveralls          # カバレッジ測定
```

### 統合コマンド
```bash
mix check              # 基本チェック (format, credo, test)
mix quality            # 全品質チェック (format, credo, test, coverage)
```

### 開発用ショートカット
```bash
mix dev                # 利用可能なコマンド一覧
mix dev.test           # テスト実行
mix dev.format         # コードフォーマット
mix dev.coverage       # HTMLカバレッジレポート生成
```

### 自動化
```bash
mix test.watch         # ファイル変更時の自動テスト実行
```

## コード品質

- **テストカバレッジ**: 83.3% (7 tests, 0 failures)
- **静的解析**: Credoルールクリア
- **コードフォーマット**: Elixir標準準拠

## アーキテクチャ

### 実装の特徴

1. **パターンマッチング**: 条件に応じた関数定義
2. **Guard**: 関数定義時の条件指定
3. **パイプライン演算子**: データ変換の流れを表現
4. **関数キャプチャ**: 高階関数での活用

### コード例

```elixir
defmodule FizzBuzz do
  # パターンマッチングとguardを活用
  def generate(number) when rem(number, 15) == 0, do: "FizzBuzz"
  def generate(number) when rem(number, 3) == 0, do: "Fizz"
  def generate(number) when rem(number, 5) == 0, do: "Buzz"
  def generate(number), do: to_string(number)

  # パイプライン演算子と関数キャプチャ
  def list(max) do
    1..max
    |> Enum.map(&generate/1)
  end
end
```

## 学習内容

このプロジェクトを通じて以下を学習できます：

### TDD（テスト駆動開発）
- Red-Green-Refactorサイクル
- 仮実装から本実装への進化
- 三角測量技法

### Elixir言語機能
- パターンマッチング
- ガード
- パイプライン演算子
- 関数キャプチャ
- Enumモジュール

### 開発ツール
- ExUnit（テストフレームワーク）
- Credo（静的解析）
- ExCoveralls（カバレッジ）
- Mix（ビルドツール）

## 開発履歴

プロジェクトはTDDの段階的な進化を示すコミット履歴で構成されています：

1. `test: セットアップ`
2. `test: 数を文字列にして返す`
3. `refactor: メソッドの抽出`
4. `test: 3を渡したら文字列Fizzを返す`
5. `test: 5を渡したら文字列Buzzを返す`
6. `test: 15を渡したら文字列FizzBuzzを返す`
7. `test: 1から100までのFizzBuzzの配列を返す`
8. `refactor: パターンマッチングとguardによる実装`
9. `feat: プリント機能の追加`
10. `chore: 静的コード解析ツールCredoの導入`
11. `chore: コードフォーマッタの設定`
12. `chore: タスクランナーとしてのMixタスク追加`
13. `chore: タスクの自動化と開発ガイド追加`

## 関連リソース

- [Elixir公式ドキュメント](https://elixir-lang.org/docs.html)
- [ExUnit documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Credo](https://github.com/rrrene/credo)
- [ExCoveralls](https://github.com/parroty/excoveralls)

## ライセンス

このプロジェクトは学習目的で作成されています。

