# Go FizzBuzz アプリケーション概要

## プロジェクトの目的

このプロジェクトは、Go言語を使用してFizzBuzzゲームを実装し、テスト駆動開発（TDD）とドメイン駆動設計（DDD）の実践的な学習を目的としています。複数のFizzBuzzタイプをサポートする拡張可能なアーキテクチャを採用しています。

## 技術スタック

- **言語**: Go 1.x
- **テストフレームワーク**: Go標準のtestingパッケージ
- **アーキテクチャパターン**: ドメイン駆動設計（DDD）
- **開発手法**: テスト駆動開発（TDD）
- **ビルドツール**: Task（Taskfile.yml）
- **コンテナ**: Docker

## 機能概要

### 基本機能
- 指定範囲の数値に対してFizzBuzzゲームを実行
- 複数のFizzBuzzタイプの実装
  - TYPE_01: 基本的なFizzBuzz（3の倍数でFizz、5の倍数でBuzz）
  - TYPE_02: カスタムFizzBuzz実装
  - TYPE_03: 拡張FizzBuzz実装
- FizzBuzzリストの生成と管理
- 各数値に対するFizzBuzz判定

### 設計原則
- **単一責任原則**: 各クラスは明確に定義された責任を持つ
- **開放閉鎖原則**: 新しいFizzBuzzタイプの追加が既存コードを変更せずに可能
- **依存性逆転原則**: 具象クラスではなく抽象に依存
- **テスト駆動開発**: 全機能がユニットテストでカバーされている

## システム構成

```
app/
├── main.go                     # エントリーポイント
├── fizzbuzz.go                 # FizzBuzzメインクラス
├── fizzbuzz_test               # テスト実行ファイル
├── main_test.go               # メインのテストファイル
├── application/               # アプリケーション層
│   ├── fizz_buzz_command.go
│   ├── fizz_buzz_list_command.go
│   └── fizz_buzz_value_command.go
└── domain/                    # ドメイン層
    ├── model/                 # ドメインモデル
    │   ├── fizz_buzz_list.go
    │   └── fizz_buzz_value.go
    └── type/                  # FizzBuzzタイプ実装
        ├── fizz_buzz_type.go
        ├── fizz_buzz_type_01.go
        ├── fizz_buzz_type_02.go
        ├── fizz_buzz_type_03.go
        └── fizz_buzz_type_not_defined.go
```

## コンポーネント詳細

### FizzBuzzメインクラス
- **責任**: FizzBuzzゲームの統合管理
- **主要機能**: 
  - FizzBuzzタイプの管理
  - リスト生成の制御
  - 結果の出力管理

### ドメインモデル
- **FizzBuzzList**: FizzBuzz値のコレクション管理
- **FizzBuzzValue**: 個別のFizzBuzz値の表現

### FizzBuzzタイプ実装
- **FizzBuzzType**: 各タイプの共通インターフェース
- **TYPE_01/02/03**: 異なるFizzBuzzルールの実装
- **NotDefined**: 未定義タイプのハンドリング

## データフロー

1. **初期化**: 指定されたタイプでFizzBuzzインスタンスを生成
2. **リスト生成**: 開始値と終了値を指定してFizzBuzzリストを作成
3. **値判定**: 各数値に対してタイプ固有の判定ロジックを実行
4. **結果出力**: 生成されたFizzBuzzリストを表示

## テスト戦略

- **ユニットテスト**: 各コンポーネントの個別テスト
- **統合テスト**: コンポーネント間の連携テスト
- **カバレッジ**: テストカバレッジの計測と維持
- **TDD**: Red-Green-Refactorサイクルの実践

このアプリケーションは、シンプルなFizzBuzzゲームを通じて、Go言語でのクリーンアーキテクチャとテスト駆動開発の実践例を提供しています。
