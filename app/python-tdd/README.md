# Python TDD FizzBuzz Implementation

「テスト駆動開発から始めるRuby入門3」記事をPythonで実装したプロジェクトです。

## プロジェクト概要

このプロジェクトは、TDD（テスト駆動開発）を通じてオブジェクト指向プログラミングの概念とデザインパターンを学習するためのものです。

### 学習内容

- **オブジェクト指向プログラム**
  - カプセル化
  - ポリモーフィズム  
  - 継承

- **オブジェクト指向設計**
  - 単一責任の原則
  - オープン・クローズドの原則
  - デザインパターンの実践

- **モジュール分割**
  - ドメインオブジェクトの抽出
  - ドメインモデルの整理
  - 清潔なアーキテクチャ

## アーキテクチャ

```
lib/
├── domain/                     # ドメイン層
│   ├── model/                  # ドメインモデル
│   │   ├── fizz_buzz_value.py  # 値オブジェクト
│   │   └── fizz_buzz_list.py   # ファーストクラスコレクション
│   └── type/                   # ドメインタイプ
│       ├── fizz_buzz_type.py           # 基底クラス + Factory Method
│       ├── fizz_buzz_type_01.py        # Type1実装
│       ├── fizz_buzz_type_02.py        # Type2実装
│       ├── fizz_buzz_type_03.py        # Type3実装
│       └── fizz_buzz_type_not_defined.py # Null Object
└── application/                # アプリケーション層
    ├── fizz_buzz_command.py            # Command インターフェース
    ├── fizz_buzz_value_command.py      # 値生成コマンド
    └── fizz_buzz_list_command.py       # リスト生成コマンド
```

## 実装されたデザインパターン

- **Value Object パターン**: FizzBuzzValue
- **Factory Method パターン**: FizzBuzzType.create()
- **Strategy パターン**: FizzBuzzType階層による型別動作
- **Command パターン**: FizzBuzzCommand階層
- **Null Object パターン**: FizzBuzzTypeNotDefined
- **First-Class Collection**: FizzBuzzList

## 実行方法

### 必要な環境

- Python 3.10+
- pytest

### テストの実行

```bash
# 全テスト実行
python3 -m pytest

# 詳細モード
python3 -m pytest -v

# 特定のテストファイル実行
python3 -m pytest test/test_integration.py -v
```

### メインアプリケーションの実行

```bash
python3 main.py
```

1から100までのFizzBuzz結果が出力されます。

## テスト構成

- **統合テスト**: `test/test_integration.py`
- **アプリケーション層テスト**: `test/application/`
- **ドメインモデルテスト**: `test/domain/model/`

総テスト数: 30個

## FizzBuzzの仕様

### Type 1 (通常のFizzBuzz)
- 3の倍数: "Fizz"
- 5の倍数: "Buzz"  
- 15の倍数: "FizzBuzz"
- その他: 数値の文字列

### Type 2 (数値のみ)
- 全て数値の文字列

### Type 3 (FizzBuzzのみ)
- 15の倍数: "FizzBuzz"
- その他: 数値の文字列

### 未定義タイプ
- 全て空文字列

## 学習成果

このプロジェクトを通じて以下を実践的に学習できます：

1. **TDD（テスト駆動開発）** の実践
2. **リファクタリング技法** の適用
3. **オブジェクト指向設計原則** の理解
4. **デザインパターン** の実装経験
5. **モジュール設計** の実践

## 参考資料

- [テスト駆動開発から始めるRuby入門3](../../../docs/wiki/記事/テスト駆動開発から始めるRuby入門3.md)
- [開発日誌](../../../docs/journal/20250711.md)
